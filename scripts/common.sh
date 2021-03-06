#!/usr/bin/env bash

base_common="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

function find_binary {
  binpath=`find $base_common/../.stack-work/install -type d -name bin | sort | tail -n1`
  echo "$binpath"/$1
}

function find_build_binary {
  binpath=`find $base_common/../.stack-work/dist -type d -name build | sort | tail -n1`
  echo "$binpath"/$1/$1
}

function ensure_run {
  run_dir="$base_common/../run"
  mkdir -p "$run_dir"
}

LOGS_TIME=`date '+%F_%H%M%S'`

function ensure_logs {
  logs_dir="$base_common/../logs/$LOGS_TIME"
  mkdir -p "$logs_dir"
}

function dump_path {
    ensure_logs
    echo -n "$logs_dir/dump/$1"
}

function logs {
  ensure_logs

  local log_file=$1
  local conf_dir="$logs_dir/conf"
  local template_name="log-template.yaml"
  if [[ "$LOG_TEMPLATE" != "" ]]; then
    template_name="$LOG_TEMPLATE"
  fi
  local template="$base_common/$template_name"

  mkdir -p "$conf_dir"
  mkdir -p "$logs_dir/dump"

  local conf_file="$conf_dir/$log_file.yaml"
  cat "$template" \
    | sed "s/{{file}}/$log_file/g" \
    > "$conf_file"
  echo -n " --json-log=$logs_dir/node$i.json "
  echo -n " --logs-prefix $logs_dir --log-config $conf_file "
}

function get_port {
  local i=$1
  local i2=$i
  if [[ $i -lt 10 ]]; then
    i2="0$i"
  fi
  echo "30$i2"
}

function dht_key {
  local i=$1
  local i2=$i
  if [[ $i -lt 10 ]]; then
    i2="0$i"
  fi

  $(find_binary "cardano-dht-keygen") 000000000000$i2 | tr -d '\n'
}

function peer_config {
  local j=$1
  echo -n " --peer 127.0.0.1:"`get_port $j`'/'`dht_key $j`
}

function dht_config {
  local i="$1"
  shift
  local j=0
  if [[ "$1" == "all" ]]; then
    n=$2
    while [[ $j -lt $n ]]; do
        peer_config $j
        j=$((j+1))
    done
    echo -n " --explicit-initial --disable-propagation"
  else
    while [[ $# -gt 0 ]]; do
      peer_config $1
      shift
    done
  fi

  if [[ "$i" != "rand" ]]; then
    echo -n " --dht-key "`dht_key $i`
  fi
}

function node_cmd {
  local i=$1
  local time_lord=$2
  local dht_cmd=$3
  local is_stat=$4
  local stake_distr=$5
  local wallet_args=$6
  local kademlia_dump_path=$7
  local st=''
  local reb=''
  local ssc_algo=''
  local web=''

  ensure_run

  keys_args="--vss-genesis $i --spending-genesis $i"
  if [[ "$CSL_PRODUCTION" != "" ]]; then
      keys_args="--keyfile \"secrets/secret-$((i+1)).key\""
  fi

  if [[ $time_lord != "" ]] && [[ $time_lord != 0 ]]; then
    time_lord=" --time-lord"
  fi
  if [[ "$SSC_ALGO" != "" ]]; then
    ssc_algo=" --ssc-algo $SSC_ALGO "
  fi
  if [[ $NO_REBUILD == "" ]]; then
    reb=" --rebuild-db "
  fi
  if [[ $is_stat != "" ]]; then
    stats=" --stats "
  fi
  if [[ "$REPORT_SERVER" != "" ]]; then
    report_server=" --report-server $REPORT_SERVER "
  fi
  if [[ $i == "0" ]]; then
    if [[ $CARDANO_WEB != "" ]]; then
      web=" --web "
    fi
  fi
  if [[ "$CSL_RTS" != "" ]] && [[ $i -eq 0 ]]; then
    rts_opts="+RTS -N -pa -A6G -qg -RTS"
  fi

  echo -n "$(find_binary cardano-node) --db-path $run_dir/node-db$i $rts_opts  $reb $keys_args"

  $dht_cmd

  # monitor_port=$((8000+$i))

  echo -n " --listen 127.0.0.1:"`get_port $i`
  echo -n " $(logs node$i.log) $time_lord $stats"
  echo -n " $stake_distr $ssc_algo "
  echo -n " $web "
  echo -n " $report_server "
  echo -n " $wallet_args "
  echo -n " --kademlia-dump-path  $(dump_path $kademlia_dump_path)"
  # echo -n " --monitor-port $monitor_port +RTS -T -RTS "
  echo ''
}

function has_nix {
    which nix-shell 2> /dev/null
    return $?
}

function stack_build {
    if [[ `has_nix` == 0 ]]; then
        echo "Building with nix-shell"
        stack --nix build --test --no-run-tests --bench --no-run-benchmarks --fast
    else
        echo "Building normally"
        stack build --test --no-run-tests --bench --no-run-benchmarks --fast
    fi
}
