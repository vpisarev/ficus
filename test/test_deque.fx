/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// tests for the deque type
from UTest import *
import Deque

TEST("deque.simple", fun() {
    val d = Deque.t {head=[1, 2, 3, 4, 5], tail=[7, 6]}
    EXPECT_EQ(`d.list()`, [1, 2, 3, 4, 5, 6, 7])
    EXPECT_EQ(`d.first()`, 1)
    EXPECT_EQ(`d.last()`, 7)
    val (x, d) = d.pop_back()
    val (y, d) = d.pop_back()
    val (z, d) = d.pop_back()
    EXPECT_EQ(`d.list()`, [1, 2, 3, 4])
    EXPECT_EQ(`x`, 7)
    EXPECT_EQ(`y`, 6)
    EXPECT_EQ(`z`, 5)
    EXPECT_EQ(`d.first()`, 1)
    EXPECT_EQ(`d.last()`, 4)
    val d = d.push_back(100)
    val d = d.push_back(200)
    val d = d.push_back(300)
    EXPECT_EQ(`d.list()`, [1, 2, 3, 4, 100, 200, 300])
    val (x, d) = d.pop_front()
    val (y, d) = d.pop_front()
    EXPECT_EQ(`d.list()`, [3, 4, 100, 200, 300])
    EXPECT_EQ(`x`, 1)
    EXPECT_EQ(`y`, 2)
    EXPECT_EQ(`d.first()`, 3)
    EXPECT_EQ(`d.last()`, 300)
    val d = d.push_front(-1)
    val d = d.push_front(-2)
    val d = d.push_front(-3)
    EXPECT_EQ(`d.list()`, [-3, -2, -1, 3, 4, 100, 200, 300])
    EXPECT_EQ(`d.length()`, 8)
    EXPECT_EQ(`f"{d.rev()}"`, "[300, 200, 100, 4, 3, -1, -2, -3]")
    EXPECT_EQ(`f"{d.map(fun (i) {i*-3})}"`, "[9, 6, 3, -9, -12, -300, -600, -900]")
    EXPECT_EQ(`d.foldl(fun (i, s) {s*i}, 1)`, -432000000)
    EXPECT_EQ(`d.foldr(fun (i, s) {s+"."+string(i)}, "0")`, "0.300.200.100.4.3.-1.-2.-3")
    EXPECT_EQ(`d.all(fun (i) {-3 <= i <= 300})`, true)
    EXPECT_EQ(`d.exists(fun (i) {i == 0})`, false)
    EXPECT_THROWS(`fun() {ignore(d.find(fun (i) {i == 0}))}`, NotFoundError)
    EXPECT_EQ(`d.find(fun (i) {i > 0})`, 3)
})
