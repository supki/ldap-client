{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "asn1";
    version = "0.2.3";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/asn1/-/asn1-0.2.3.tgz";
      sha1 = "dac8787713c9966849fc8180777ebe9c1ddf3b86";
    };
    deps = [];
    meta = {
      homepage = "https://github.com/mcavage/node-asn1";
      description = "Contains parsers and serializers for ASN.1 (currently BER only)";
    };
  }