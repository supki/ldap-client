{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "pooling";
    version = "0.4.6";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/pooling/-/pooling-0.4.6.tgz";
      sha1 = "76a317371ea8a363b4858fa4799e60245f30e664";
    };
    deps = with nodePackages; [
      bunyan_0-22-1
      once_1-3-0
      vasync_1-4-0
      dtrace-provider_0-2-8
      assert-plus_0-1-5
    ];
    optionalDependencies = with nodePackages; [
      dtrace-provider_0-2-8
    ];
    meta = {
      homepage = "https://github.com/mcavage/node-pooling";
      description = "General purpose resource pool API";
    };
  }