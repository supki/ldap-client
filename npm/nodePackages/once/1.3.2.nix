{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "once";
    version = "1.3.2";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/once/-/once-1.3.2.tgz";
      sha1 = "d8feeca93b039ec1dcdee7741c92bdac5e28081b";
    };
    deps = with nodePackages; [
      wrappy_1-0-1
    ];
    meta = {
      homepage = "https://github.com/isaacs/once#readme";
      description = "Run a function exactly one time";
      keywords = [
        "once"
        "function"
        "one"
        "single"
      ];
    };
  }