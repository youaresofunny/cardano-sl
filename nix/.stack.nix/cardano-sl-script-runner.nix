{ system
, compiler
, flags
, pkgs
, hsPkgs
, pkgconfPkgs
, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = {
        name = "cardano-sl-script-runner";
        version = "2.0.0";
      };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - Script Runner";
      description = "Cardano SL - ScriptRunner";
      buildType = "Simple";
    };
    components = {
      exes = {
        "poc" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.ansi-wl-pprint)
            (hsPkgs.brick)
            (hsPkgs.aeson)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-binary)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-client)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.cardano-sl-db)
            (hsPkgs.cardano-sl-generator)
            (hsPkgs.cardano-sl-infra)
            (hsPkgs.cardano-sl-networking)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.cborg)
            (hsPkgs.conduit)
            (hsPkgs.constraints)
            (hsPkgs.containers)
            (hsPkgs.data-default)
            (hsPkgs.dns)
            (hsPkgs.formatting)
            (hsPkgs.lens)
            (hsPkgs.lifted-async)
            (hsPkgs.mtl)
            (hsPkgs.optparse-applicative)
            (hsPkgs.process)
            (hsPkgs.reflection)
            (hsPkgs.resourcet)
            (hsPkgs.serokell-util)
            (hsPkgs.stm)
            (hsPkgs.text)
            (hsPkgs.time)
            (hsPkgs.time-units)
            (hsPkgs.transformers)
            (hsPkgs.turtle)
            (hsPkgs.universum)
            (hsPkgs.unix)
            (hsPkgs.unordered-containers)
            (hsPkgs.vty)
            (hsPkgs.yaml)
          ];
        };
      };
    };
  } // rec {
    src = .././../script-runner;
  }