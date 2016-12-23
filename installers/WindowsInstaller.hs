import Development.NSIS
import Development.NSIS.Plugins.EnvVarUpdate
import Turtle
import System.Directory


writeNSIS :: IO ()
writeNSIS = writeFile "cardano.nsi" $ nsis $ do
     name "Cardano"                  -- The name of the installer
     outFile "cardano-win64-installer-${version}.exe"           -- Where to produce the installer
     installDir "$APPDATA\\Cardano"   -- The default installation directory
     installDirRegKey HKCU "Software/Cardano" "Install_Dir"
     requestExecutionLevel User       -- Request application privileges for Windows Vista

     page Directory                   -- Pick where to install
     page InstFiles                   -- Give a progress bar while installing

     section "" [Required] $ do
         setEnvVarPrepend HKCU "PATH" "$INSTDIR"
         setOutPath "$INSTDIR"        -- Where to install files in this section
         file [] "cardano-node.exe"
         file [] "rocksdb.dll"
         file [] "libstdc++-6.dll"
         file [] "libwinpthread-1.dll"
         file [] "libgcc_s_seh-1.dll"
         file [] "advapi32.dll"
         file [] "version.txt"

mingwRoot :: Prelude.FilePath
mingwRoot = "C:\\msys64\\mingw64\\bin\\"

main :: IO ()
main = do
  copyFile (mingwRoot <> "libstdc++-6.dll") "libstdc++-6.dll"
  copyFile (mingwRoot <> "libgcc_s_seh-1.dll") "libgcc_s_seh-1.dll"
  copyFile (mingwRoot <> "libwinpthread-1.dll") "libwinpthread-1.dll"
  copyFile "C:\\Windows\\System32\\advapi32.dll" "advapi32.dll"
  echo "Writing cardano.nsi"
  writeNSIS
  echo "Generating NSIS installer cardano-win64-installer.exe"
  procs "makensis" ["cardano.nsi" "/Dversion=\"%APPVEYOR_BUILD_VERSION%\""] mempty
