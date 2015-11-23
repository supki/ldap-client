{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "mv";
    version = "2.0.3";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/mv/-/mv-2.0.3.tgz";
      sha1 = "e9ab707d71dc38de24edcc637a8e2f5f480c7f32";
    };
    deps = with nodePackages; [
      ncp_0-6-0
      mkdirp_0-5-1
      rimraf_2-2-8
    ];
    meta = {
      homepage = "https://github.com/andrewrk/node-mv";
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