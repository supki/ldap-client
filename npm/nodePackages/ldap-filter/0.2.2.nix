{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "ldap-filter";
    version = "0.2.2";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/ldap-filter/-/ldap-filter-0.2.2.tgz";
      sha1 = "f2b842be0b86da3352798505b31ebcae590d77d0";
    };
    deps = with nodePackages; [
      assert-plus_0-1-5
    ];
    meta = {
      homepage = "http://ldapjs.org";
      description = "API for handling LDAP-style filters";
    };
  }