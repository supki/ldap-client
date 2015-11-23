{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "ldapjs";
    version = "1.0.0";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/ldapjs/-/ldapjs-1.0.0.tgz";
      sha1 = "1da2cd5bfb9cb103c1ba516938da971bc2bbc3f2";
    };
    deps = with nodePackages; [
      ldap-filter_0-2-2
      asn1_0-2-3
      bunyan_1-5-1
      once_1-3-2
      vasync_1-6-3
      dtrace-provider_0-6-0
      backoff_2-4-1
      assert-plus_0-1-5
      verror_1-6-0
      dashdash_1-10-1
    ];
    optionalDependencies = with nodePackages; [
      dtrace-provider_0-6-0
    ];
    meta = {
      homepage = "http://ldapjs.org";
      description = "LDAP client and server APIs";
    };
  }