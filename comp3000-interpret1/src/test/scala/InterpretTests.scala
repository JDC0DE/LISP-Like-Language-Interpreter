/*
 * This file is part of COMP3000 assignment 1.
 *
 * Copyright (C) 2021 Kym Haines, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Tests of the Snake puzzle solver.
 * Uses the ScalaTest `FlatSpec` style for writing tests. See
 *
 *      http://www.scalatest.org/user_guide
 *
 * For more info on writing ScalaTest tests.
 */

package org.mq.interpret

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class TokensTests extends FlatSpec with Matchers {

  import Interpret._

  "PARSING: pattern match" should "handle simple list" in {
    assert(matchPat("(a b c)") == List("(", "a", "b", "c", ")"))
  }

  it should "handle numbers" in {
    assert(matchPat("(14 25 -7 834 9)") ==
                          List("(", "14", "25", "-7", "834", "9", ")"))
  }

  it should "handle operators" in {
    assert(matchPat("(+ 3 (* 4 5))") ==
                          List("(", "+", "3", "(", "*", "4", "5", ")", ")"))
  }

  "strToLObj" should "handle a name" in {
    assert(strToLObj("joe43") == LSymbol("joe43"))
  }

  it should "handle a number" in {
    assert(strToLObj("2021") == LNumber(2021))
  }

  it should "handle a negative number" in {
    assert(strToLObj("-53") == LNumber(-53))
  }

  it should "handle an operator" in {
    assert(strToLObj("+") == LSymbol("+"))
  }

  "tokensToLObjs" should "handle a single name" in {
    assert(tokensToLObjs(List("abc")) == Some(LSymbol("abc")))
  }

  it should "handle a single number" in {
    assert(tokensToLObjs(List("42")) == Some(LNumber(42)))
  }

  it should "handle an empty list" in {
    assert(tokensToLObjs(List("(", ")")) == Some(LSymbol("nil")))
  }

  it should "handle a simple list" in {
    assert(tokensToLObjs(List("(", "a", "b", "g", ")")) ==
            Some(LList(LSymbol("a"),
                       LList(LSymbol("b"),
                             LList(LSymbol("g"),
                                   LSymbol("nil"))))))
  }

  it should "handle a more-complex list" in {
    assert(tokensToLObjs(List("(", "a", "(", "b", ")", "g", ")")) ==
            Some(LList(LSymbol("a"),
                       LList(LList(LSymbol("b"), LSymbol("nil")),
                             LList(LSymbol("g"),
                                   LSymbol("nil"))))))
  }

  "FUNCTIONS: add" should "add two numbers" in {
    assert(add(LNumber(4), LNumber(3)) == LNumber(7))
  }

  it should "detect non-numbers" in {
    assert(add(LNumber(4), LSymbol("a")) == LSymbol("ERROR"))
  }

  "sub" should "subtract two numbers" in {
    assert(sub(LNumber(4), LNumber(3)) == LNumber(1))
  }

  "mul" should "multiply two numbers" in {
    assert(mul(LNumber(4), LNumber(3)) == LNumber(12))
  }

  "div" should "divde two numbers" in {
    assert(div(LNumber(12), LNumber(3)) == LNumber(4))
  }

  "car" should "get the head of a list" in {
    // car of (a b g) is a
    assert(car(LList(LSymbol("a"),
                     LList(LSymbol("b"),
                           LList(LSymbol("g"),
                                 LSymbol("nil"))))) == LSymbol("a"))
  }

  "cdr" should "get the tail of a list" in {
    // cdr of (a b g) is (b g)
    assert(cdr(LList(LSymbol("a"),
                     LList(LSymbol("b"),
                           LList(LSymbol("g"),
                                 LSymbol("nil")))))
               == LList(LSymbol("b"),
                        LList(LSymbol("g"),
                              LSymbol("nil"))))
  }

  "cons" should "add the first arg to the front of the second arg list" in {
    // cons of a and (b c) is (a b c)
    assert(cons(LSymbol("a"), LList(LSymbol("b"),
                                    LList(LSymbol("c"),LSymbol("nil"))))
           == LList(LSymbol("a"),
                    LList(LSymbol("b"),LList(LSymbol("c"),LSymbol("nil")))))
  }

  "eeqq" should "give t if equal" in {
    assert(eeqq(LSymbol("a"), LSymbol("a")) == LSymbol("t"))
  }

  it should "give nil if not equal" in {
    assert(eeqq(LSymbol("a"), LSymbol("b")) == LSymbol("nil"))
  }

  "iiff" should "do 2nd arg if first arg is true" in {
    // if of t, (+ 3 4), (- 5 2) is 7
    assert(iiff(LSymbol("t"),
                LList(LSymbol("+"),
                      LList(LNumber(3),LList(LNumber(4),LSymbol("nil")))),
                LList(LSymbol("-"),
                      LList(LNumber(5),LList(LNumber(2),LSymbol("nil")))))
           == LNumber(7))
  }

  it should "do 3rd arg if first arg is false" in {
    // if of nil, (+ 3 4), (- 5 2) is 3
    assert(iiff(LSymbol("nil"),
                LList(LSymbol("+"),
                      LList(LNumber(3),LList(LNumber(4),LSymbol("nil")))),
                LList(LSymbol("-"),
                      LList(LNumber(5),LList(LNumber(2),LSymbol("nil")))))
           == LNumber(3))
  }

  "EVALUATING: eval" should "handle nil" in {
    assert(eval(LSymbol("nil")) == LSymbol("nil"))
  }

  it should "handle t" in {
    assert(eval(LSymbol("t")) == LSymbol("t"))
  }

  it should "handle a number" in {
    assert(eval(LNumber(51)) == LNumber(51))
  }

  it should "handle quote of symbol: (quote a)" in {
    assert(eval(LList(LSymbol("quote"),LList(LSymbol("a"),LSymbol("nil"))))
           == LSymbol("a"))
  }

  it should "handle quote of list: (quote (a b c))" in {
    assert(eval(LList(LSymbol("quote"),
                      LList(LList(LSymbol("a"),
                                  LList(LSymbol("b"),
                                        LList(LSymbol("c"),LSymbol("nil")))),
                            LSymbol("nil"))))
          == LList(LSymbol("a"),
                   LList(LSymbol("b"),LList(LSymbol("c"),LSymbol("nil")))))
  }

  "arithmetic" should "handle +: (+ 3 4)" in {
    assert(eval(LList(LSymbol("+"),
                      LList(LNumber(3),LList(LNumber(4),LSymbol("nil")))))
           == LNumber(7))
  }

  it should "handle -: (- (+ 7 2) (- 4 1))" in {
    assert(eval(LList(LSymbol("-"),
                      LList(LList(LSymbol("+"),
                                  LList(LNumber(7),
                                        LList(LNumber(2),LSymbol("nil")))),
                            LList(LList(LSymbol("-"),
                                        LList(LNumber(4),
                                              LList(LNumber(1),
                                                    LSymbol("nil")))),
                                  LSymbol("nil")))))
           == LNumber(6))
  }

  it should "handle complex *,/: (* (car (cdr (quote (u 4 v)))) (/ (/ 27 3) 3))" in {
    assert(eval(LList(LSymbol("*"),
                      LList(LList(LSymbol("car"),
                                  LList(LList(LSymbol("cdr"),
                                              LList(LList(LSymbol("quote"),
                                                LList(LList(LSymbol("u"),
                                                  LList(LNumber(4),
                                                    LList(LSymbol("v"),
                                                      LSymbol("nil")))),
                                                LSymbol("nil"))),
                                            LSymbol("nil"))),
                                        LSymbol("nil"))),
                                    LList(LList(LSymbol("/"),
                                      LList(LList(LSymbol("/"),
                                        LList(LNumber(27),
                                          LList(LNumber(3),
                                            LSymbol("nil")))),
                                      LList(LNumber(3),LSymbol("nil")))),
                                  LSymbol("nil")))))
           == LNumber(12))
  }

  "list operations" should "handle car: (car (quote (a b c)))" in {
    assert(eval(LList(LSymbol("car"),
                      LList(LList(LSymbol("quote"),
                                  LList(LList(LSymbol("a"),
                                              LList(LSymbol("b"),
                                                    LList(LSymbol("c"),
                                                          LSymbol("nil")))),
                                        LSymbol("nil"))),
                            LSymbol("nil"))))
           == LSymbol("a"))
  }

  it should "handle cdr: (cdr (quote (a b c)))" in {
    assert(eval(LList(LSymbol("cdr"),
                      LList(LList(LSymbol("quote"),
                                  LList(LList(LSymbol("a"),
                                              LList(LSymbol("b"),
                                                    LList(LSymbol("c"),
                                                          LSymbol("nil")))),
                                        LSymbol("nil"))),
                            LSymbol("nil"))))
           == LList(LSymbol("b"),LList(LSymbol("c"),LSymbol("nil"))))
  }

  it should "handle cons: (cons (quote a) (quote (b c)))" in {
    assert(eval(LList(LSymbol("cons"),
                      LList(LList(LSymbol("quote"),
                                  LList(LSymbol("a"),
                                        LSymbol("nil"))),
                            LList(LList(LSymbol("quote"),
                                        LList(LList(LSymbol("b"),
                                                    LList(LSymbol("c"),
                                                          LSymbol("nil"))),
                                              LSymbol("nil"))),
                                  LSymbol("nil")))))
           == LList(LSymbol("a"),
                    LList(LSymbol("b"),
                          LList(LSymbol("c"),
                                LSymbol("nil")))))
  }

  "conditions" should "handle eq for false condition: (eq 5 8)" in {
    assert(eval(LList(LSymbol("eq"),
                      LList(LNumber(5),
                            LList(LNumber(8), LSymbol("nil")))))
           == LSymbol("nil"))
  }

  it should "handle eq for true condition: (eq (quote a) (quote a))" in {
    assert(eval(LList(LSymbol("eq"),
                      LList(LList(LSymbol("quote"),
                                  LList(LSymbol("a"),LSymbol("nil"))),
                            LList(LList(LSymbol("quote"),
                                        LList(LSymbol("a"),LSymbol("nil"))),
                                  LSymbol("nil")))))
           == LSymbol("t"))
  }

  it should "handle if when condition false: (if nil 3 4)" in {
    assert(eval(LList(LSymbol("if"),
                      LList(LSymbol("nil"),
                            LList(LNumber(3),
                                  LList(LNumber(4),LSymbol("nil"))))))
           == LNumber(4))
  }

  it should "handle if when condition true: (if t 3 4)" in {
    assert(eval(LList(LSymbol("if"),
                      LList(LSymbol("t"),
                            LList(LNumber(3),
                                  LList(LNumber(4),LSymbol("nil"))))))
           == LNumber(3))
  }

  it should "handle if with expressions: (if (eq 3 3) (+ 2 3) (* 4 5))" in {
    assert(eval(LList(LSymbol("if"),
                      LList(LList(LSymbol("eq"),
                                  LList(LNumber(3),
                                        LList(LNumber(3),LSymbol("nil")))),
                            LList(LList(LSymbol("+"),
                                        LList(LNumber(2),
                                              LList(LNumber(3),
                                                    LSymbol("nil")))),
                                  LList(LList(LSymbol("*"),
                                              LList(LNumber(4),
                                                    LList(LNumber(5),
                                                          LSymbol("nil")))),
                                        LSymbol("nil"))))))
           == LNumber(5))
  }

  "eval of eval" should "work: (eval (quote (+ 12 45)))" in {
    assert(eval(LList(LSymbol("eval"),
                      LList(LList(LSymbol("quote"),
                                  LList(LList(LSymbol("+"),
                                              LList(LNumber(12),
                                                    LList(LNumber(45),
                                                          LSymbol("nil")))),
                                        LSymbol("nil"))),
                            LSymbol("nil"))))
           == LNumber(57))
  }

  "variables" should "assign to a variable" in {
    assert({
             resetEnv
             // (setq x 12)
             eval(LList(LSymbol("setq"),
                        LList(LSymbol("x"),
                              LList(LNumber(12),LSymbol("nil")))))
             // x
             eval(LSymbol("x"))
           }   == LNumber(12))
  }

  it should "use a variable in an expression" in {
    assert({
             resetEnv
             // (setq s (quote (g h k)))
             eval(LList(LSymbol("setq"),
                        LList(LSymbol("s"),
                              LList(LList(LSymbol("quote"),
                                          LList(LList(LSymbol("g"),
                                            LList(LSymbol("h"),
                                              LList(LSymbol("k"),
                                                LSymbol("nil")))),
                                          LSymbol("nil"))),
                                    LSymbol("nil")))))
             // (car s)
             eval(LList(LSymbol("car"),LList(LSymbol("s"),LSymbol("nil"))))
           }   == LSymbol("g"))
  }

  "user functions" should "define and use a function" in {
    assert({
             resetEnv
             // (def foo(y)(+ y 2))
             eval(LList(LSymbol("def"),
                        LList(LSymbol("foo"),
                              LList(LList(LSymbol("y"),LSymbol("nil")),
                                    LList(LList(LSymbol("+"),
                                                LList(LSymbol("y"),
                                                      LList(LNumber(2),
                                                            LSymbol("nil")))),
                                          LSymbol("nil"))))))
             // (foo 4)
             eval(LList(LSymbol("foo"),LList(LNumber(4),LSymbol("nil"))))
           }   == LNumber(6))
  }

  "recursive user functions" should "define and use a recursive function" in {
    assert({
             resetEnv
             // (def sum(y)(if (eq y 0) 0 (+ y (sum (- y 1)))))
             eval(LList(LSymbol("def"),
                        LList(LSymbol("sum"),
                              LList(LList(LSymbol("y"),LSymbol("nil")),
                                LList(LList(LSymbol("if"),
                                  LList(LList(LSymbol("eq"),
                                    LList(LSymbol("y"),LList(LNumber(0),
                                      LSymbol("nil")))),LList(LNumber(0),
                                  LList(LList(LSymbol("+"),LList(LSymbol("y"),
                                    LList(LList(LSymbol("sum"),
                                      LList(LList(LSymbol("-"),
                                        LList(LSymbol("y"),LList(LNumber(1),
                                          LSymbol("nil")))),
                                    LSymbol("nil"))),
                                LSymbol("nil")))),
                          LSymbol("nil"))))),
                    LSymbol("nil"))))))
             // (sum 4)
             eval(LList(LSymbol("sum"),LList(LNumber(4),LSymbol("nil"))))
           }   == LNumber(10))
  }

  ///////////////////////////////////////////////////////////////////
  //
  // TO DO     put your test case below
  //
  ///////////////////////////////////////////////////////////////////
   it should "regex big list" in {
    assert(matchPat("(+ -9 (+ -6 (- -5 7)))") ==
                          List("(", "+", "-9", "(", "+", "-6", "(", "-", "-5", "7", ")", ")", ")"))
  }

  it should "error list" in {
    assert(tokensToLObjs(List(")")) == Some(error))
  }

  it should "handle a big list with nums and chars" in {
    assert(tokensToLObjs(List("(", "-7", "x", "5", "4", "y", "z", "2", ")")) ==
            Some(LList(LNumber(-7),
                       LList(LSymbol("x"),
                             LList(LNumber(5),
                                  LList(LNumber(4),
                                        LList(LSymbol("y"),
                                            LList(LSymbol("z"),
                                                LList(LNumber(2),
                                                      LSymbol("nil"))))))))))
  }
  "cons" should "adds a list to the front of a list" in {
    assert(cons(LList(LNumber(1), LList(LNumber(-5), LSymbol("nil"))), LList(LSymbol("x"),
                                    LList(LSymbol("y"),LSymbol("nil"))))
           == LList(LList(LNumber(1), LList(LNumber(-5), LSymbol("nil"))),
                    LList(LSymbol("x"),LList(LSymbol("y"),LSymbol("nil")))))
  }

  "cons" should "add a number to the front of a list" in {
    
    assert(cons(LNumber(69), LList(LSymbol("x"),
                                    LList(LSymbol("y"),LSymbol("nil"))))
           == LList(LNumber(69),
                    LList(LSymbol("x"),LList(LSymbol("y"),LSymbol("nil")))))
  }

  "eeqq" should "true for both nums the same" in {
    assert(eeqq(LNumber(2), LNumber(2)) == LSymbol("t"))
  }

  "eeqq" should "false for both nums not the same" in {
    assert(eeqq(LNumber(23), LNumber(2)) == LSymbol("nil"))
  }


  "iiff" should "3rd argument if 1st arg is false" in {
    //if nil (3)(5) == 5
    assert(iiff(LSymbol("nil"),
                      LList(LNumber(3),LSymbol("nil")),
                      LList(LNumber(5),LSymbol("nil")))
           == LNumber(5))
  }
  
  it should "true returns x" in {
    assert(eval(LList(LSymbol("if"),
                      LList(LSymbol("t"),
                            LList(LSymbol("x"),
                                  LList(LNumber(4),LSymbol("nil"))))))
           == LSymbol("x"))
  }

  it should "false returns y" in {
    assert(eval(LList(LSymbol("if"),
                      LList(LSymbol("nil"),
                            LList(LNumber(4),
                                  LList(LSymbol("y"),LSymbol("nil"))))))
           == LSymbol("y"))
  }
  
  "list operations" should "eval of quote with addition" in {
    assert(eval(LList(LSymbol("eval"),
                      LList(LList(LSymbol("quote"),
                                  LList(LList(LSymbol("+"),
                                              LList(LNumber(1),
                                                    LList(LNumber(2),
                                                          LSymbol("nil")))),
                                        LSymbol("nil"))),
                            LSymbol("nil"))))
           == LNumber(3))
  }
  "list operations" should "eval of quote with subtraction" in {
    assert(eval(LList(LSymbol("eval"),
                      LList(LList(LSymbol("quote"),
                                  LList(LList(LSymbol("-"),
                                              LList(LNumber(1),
                                                    LList(LNumber(-3),
                                                          LSymbol("nil")))),
                                        LSymbol("nil"))),
                            LSymbol("nil"))))
           == LNumber(4))
  }

   it should "complicated if test" in {
    assert(eval(LList(LSymbol("if"),
                      LList(LList(LSymbol("eq"),
                                  LList(LSymbol("a"),
                                        LList(LSymbol("a"),LSymbol("nil")))),
                            
                      LList(LList(LSymbol("car"),
                                  LList(LList(LSymbol("cdr"),
                                              LList(LList(LSymbol("quote"),
                                                LList(LList(LSymbol("x"),
                                                  LList(LNumber(6),
                                                    LList(LSymbol("y"),
                                                      LSymbol("nil")))),
                                                LSymbol("nil"))),
                                            LSymbol("nil"))),
                                        LSymbol("nil"))),
           
                                  LList(LList(LSymbol("*"),
                                              LList(LNumber(5),
                                                    LList(LNumber(6),
                                                          LSymbol("nil")))),
                                        LSymbol("nil"))))))
           == LNumber(6))
  }

  it should "eq true " in {
    assert(eval(LList(LSymbol("eq"),
                      LList(LList(LSymbol("quote"),
                                  LList(LSymbol("x"), LList(LSymbol("y"), LList(LSymbol("z"), LSymbol("nil"))))),
                            LList(LList(LSymbol("quote"),
                                        LList(LSymbol("x"), LList(LSymbol("y"), LList(LSymbol("z"), LSymbol("nil"))))),
                                  LSymbol("nil")))))
           == LSymbol("t"))
  }

  it should "eq false " in {
    assert(eval(LList(LSymbol("eq"),
                      LList(LList(LSymbol("quote"),
                                  LList(LNumber(-4), LList(LSymbol("y"), LList(LSymbol("z"), LSymbol("nil"))))),
                            LList(LList(LSymbol("quote"),
                                        LList(LNumber(4), LList(LSymbol("y"), LList(LSymbol("z"), LSymbol("nil"))))),
                                  LSymbol("nil")))))
           == LSymbol("nil"))
  }
}
