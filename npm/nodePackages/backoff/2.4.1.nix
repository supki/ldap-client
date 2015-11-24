{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "backoff";
    version = "2.4.1";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/backoff/-/backoff-2.4.1.tgz";
      sha1 = "2f68c50e0dd789dbefe24200a62efb04d2456d68";
    };
    deps = with nodePackages; [
      precond_0-2-3
    ];
    meta = {
      description = "Fibonacci and exponential backoffs.";
      keywords = [
        "backoff"
        "retry"
        "fibonacci"
        "exponential"
      ];
    };
  }