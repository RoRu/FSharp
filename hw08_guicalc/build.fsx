#! packages/FAKE/tools/FAKE.exe
#r "packages/FAKE/tools/FakeLib.dll"

open Fake

RestorePackages()

let buildDir = "./build/"
let testDir  = "./test/"

Target "Clean" (fun _ -> 
  CleanDir buildDir
  CleanDir testDir
)

Target "BuildApp" (fun _ ->
  !! "src/app/*.fsproj"
    |> MSBuildRelease buildDir "Build"
    |> Log "AppBuild-Output: "
)

Target "BuildTest" (fun _ ->
  !! "src/test/*.fsproj"
    |> MSBuildDebug testDir "Build"
    |> Log "BuildTest-Output: "
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

RunTargetOrDefault "Test"