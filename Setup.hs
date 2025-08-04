import Distribution.Simple
import Distribution.Simple.UserHooks
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.Simple.BuildPaths
import Distribution.Utils.Path (getSymbolicPath)
import System.Process (rawSystem)
import System.Directory (createDirectoryIfMissing, setCurrentDirectory)
import System.Directory (createDirectoryIfMissing, setCurrentDirectory, copyFile)
import System.FilePath ((</>))

main :: IO ()
main = defaultMainWithHooks customHooks

cSourceDirectory :: String
cSourceDirectory = "csrc"

cBuildDirectory :: String
cBuildDirectory = cSourceDirectory ++ "/build_release"

customHooks :: UserHooks
customHooks = simpleUserHooks {
    preBuild = preBuildHook,
    postBuild = postBuildHook
}

preBuildHook :: Args -> BuildFlags -> IO HookedBuildInfo
preBuildHook args buildFlags = do
    putStrLn "[PreBuild] Building C library..."

    createDirectoryIfMissing True cBuildDirectory
    setCurrentDirectory cSourceDirectory
    rawSystem "bash" ["./build.sh", "release"]
    putStrLn "C library built successfully."
    setCurrentDirectory ".."

    preBuild simpleUserHooks args buildFlags

postBuildHook :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postBuildHook args buildFlags packageDescription localBuildInfo = do
    putStrLn "[PostBuild] Copying artifacts..."

    let packageBuildDir = buildDir localBuildInfo
    -- TODO: This name will vary depending on which os the user is building on.
    let artifactName = "libelmlike.dylib"
    copyFile (cBuildDirectory </> artifactName) ((getSymbolicPath packageBuildDir) </> artifactName)

    postBuild simpleUserHooks args buildFlags packageDescription localBuildInfo
