import Distribution.Simple
import Distribution.Simple.Setup (TestFlags(..), defaultTestFlags)
import Distribution.Simple.LocalBuildInfo (buildDir)
import Distribution.PackageDescription (PackageDescription(..), TestSuite(..), TestSuiteInterface(..))
import System.FilePath ((</>))

main = defaultMainWithHooks $ simpleUserHooks {
  runTests = \pkg lbi hooks flags -> do
    let t = testSuite $ localPkgDescr lbi
        testDir = buildDir lbi </> testName t
    createDirectoryIfMissing False testDir
    writeFile (testDir </> "Spec.hs") $ unlines [
      "import Test.Hspec",
      "import BookSpec",
      "import LibrarySpec",
      "import MemberSpec",
      "main :: IO ()",
      "main = hspec $ do",
      "  describe \"BookSpec\" BookSpec.spec",
      "  describe \"LibrarySpec\" LibrarySpec.spec",
      "  describe \"MemberSpec\" MemberSpec.spec"
    ]
    defaultMainWithHooks hooks { runTests = defaultRunTests }
}
