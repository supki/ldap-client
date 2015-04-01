guard :haskell, all_on_start: true, all_on_pass: true, cmd: "cabal exec -- ghci -isrc -itest -DTEST test/Main.hs -ignore-dot-ghci -optP-include -optPdist/build/autogen/cabal_macros.h" do
  watch(%r{test/.+Spec\.l?hs$})
  watch(%r{src/.+\.l?hs$})
  watch(%r{.+\.cabal$})
end
