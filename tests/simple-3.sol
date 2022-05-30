pragma solidity ^0.8.4;

contract C {
    function f(uint256 x, uint256 y) public pure {
        uint256 dummy = 0x1234567890;
        uint256 z = 0xdeadbeef;
        if (x != 0) {
            z = x * y;
        }
    }
}
