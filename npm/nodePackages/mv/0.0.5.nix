{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "mv";
    version = "0.0.5";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/mv/-/mv-0.0.5.tgz";
      sha1 = "15eac759479884df1131d6de56bce20b654f5391";
    };
    deps = [];
    meta = {
      description = "fs.rename but works across devices. same as the unix utility 'mv'";
      keywords = [
        "mv"
        "move"
        "rename"
        "device"
        "recursive"
        "folder"
      ];
    };
  }