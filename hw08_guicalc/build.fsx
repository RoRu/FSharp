#! tools/FAKE/tools/FAKE.exe
#r "tools/FAKE/tools/FakeLib.dll"

open Fake

RestorePackages()


let buildDir = "./bin/build/"
let testDir  = "./bin/test/"


Target "Clean" (fun _ ->
    CleanDirs [buildDir; testDir]
)

Target "BuildApp" (fun _ ->
   !! "src/app/**/*.fsproj"
     |> MSBuildRelease buildDir "Build"
     |> Log "AppBuild-Output: "
)

Target "BuildTest" (fun _ ->
    !! "src/test/**/*.fsproj"
      |> MSBuildDebug testDir "Build"
      |> Log "TestBuild-Output: "
)

Target "Test" (fun _ ->
    !! (testDir + "/*.dll")
      |> NUnit (fun p ->
          {p with
             DisableShadowCopy = true;
             OutputFile = testDir + "TestResults.xml" })
)

"Clean"
  ==> "BuildApp"
  ==> "BuildTest"
  ==> "Test"

RunTargetOrDefault "Default"