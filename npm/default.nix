{ nodejsVersion ? "4.1", npm3 ? false, pkgs ? import <nixpkgs> {} }:
let
    nodeLib = import ./nodeLib {
      inherit pkgs npm3 nodejsVersion;
      self = nodeLib;
    };
    in nodeLib.generatePackages {
      rootPath = ./nodePackages;
    }
