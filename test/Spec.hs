import System.Process

main :: IO ()
main = do
    --putStrLn "\ndone"
    --system "bash t.sh"
    system "/var/lib/snapd/snap/bin/clash --verilog test/lambdaMachine.hs"
    --system "clash --vhdl src/CPU.hs"
    --system "clash --systemverilog src/CPU.hs"
    return ()
