{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "asn1";
    version = "0.2.1";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/asn1/-/asn1-0.2.1.tgz";
      sha1 = "ecc73f75d31ea3c6ed9d47428db35fecc7b2c6dc";
    };
    deps = [];
    meta = {
      homepage = "https://github.com/mcavage/node-asn1";
      description = "Contains parsers and serializers for ASN.1 (currently BER only)";
    };
  }