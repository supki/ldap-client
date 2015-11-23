{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "ldapjs";
    version = "0.7.1";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/ldapjs/-/ldapjs-0.7.1.tgz";
      sha1 = "684798a687640bab1afbd802cf532f30492dfb56";
    };
    deps = with nodePackages; [
      asn1_0-2-1
      nopt_2-1-1
      bunyan_0-22-1
      dtrace-provider_0-2-8
      pooling_0-4-6
      assert-plus_0-1-5
    ];
    optionalDependencies = with nodePackages; [
      dtrace-provider_0-2-8
    ];
    meta = {
      homepage = "http://ldapjs.org";
      description = "LDAP client and server APIs";
    };
  }