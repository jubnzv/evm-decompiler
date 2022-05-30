pragma solidity 0.8.13;

contract Test {
    function double(uint256 a) public returns (uint256) {
        return multiply(a, 2);
    }

    function triple(uint256 a) public returns (uint256) {
        return multiply(a, 3);
    }

    function multiply(uint256 a, uint256 b) internal returns (uint256) {
        return a * b;
    }
}
