/*
 * This file is part of COMP3000 assignment 1.
 *
 * Copyright (C) 2021 Kym Haines, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

// https://www.scala-lang.org/api/2.12.8/scala/util/matching/Regex.html
// https://www.scala-lang.org/api/2.12.8/scala/collection/immutable/List.html
// https://www.scala-lang.org/api/2.12.8/scala/collection/immutable/StringLike.html
// https://www.scala-lang.org/api/2.12.8/scala/collection/immutable/Map.html

package org.mq.interpret

import scala.util.matching._
import cats.instances.int
import cats.instances.list
import scala.annotation.tailrec

object Interpret {

  sealed abstract class LObject
  case class LSymbol(sym: String) extends LObject
  case class LNumber(num: Int) extends LObject
  case class LList(head: LObject, tail: LObject) extends LObject

  val nil = LSymbol("nil")
  val T = LSymbol("t")
  val error = LSymbol("ERROR")
  var reser: Int = 0
  var gloStr: String = ""

  def resetEnv: Unit = {
    reser = 0;

    // TO DO: reset your functions/variables environment

  }

  val pat = "[()]|[\\+\\*\\/\\-][0-9]*|[a-z]+|-?[0-9]+".r

  def matchPat(line: String): List[String] =
    pat.findAllIn(line.toLowerCase).toList

  // excludes brackets
  def strToLObj(s: String): LObject = { //checks the input string for any particular characters and returns the associated LObject type
    val ch = s.head
    if (ch.isLetter) LSymbol(s)
    else if (ch.isDigit) LNumber(s.toInt)
    else if (s.contains("-")) LNumber(s.toInt)
    else if (s == "-") LSymbol(s)
    else if (s == "+") LSymbol(s)
    else if (s == "*") LSymbol(s)
    else if (s == "/") LSymbol(s)
    else error
    // TO DO: convert a string token to an LObject; or error

  }

  def tokensToLObjs(a: List[String]): Option[LObject] = {
    // TO DO: convert a list of token strings to an LObject
    // NOTE: anywhere () is seen, it should be interpreted as nil
    //takes the input list and checks the elements for certain symbols before converting them to their associated LObject
    val hold = a.reverse
    var res: LObject = nil
    var tl: LObject = nil
    var temp: LObject = nil
    var tail: LObject = nil
    var head: LObject = nil
    var result: Option[LObject] = Some(nil)
    for (elements <- hold) {
      if (hold.size == 1) {
        res = elements match {
          case ")" => error
          case "(" => error
          case _   => strToLObj(elements)
        }
        return Some(res)
      }
      if (hold.size == 2) {
        res = elements match {
          case ")" => LSymbol("nil")
          case "(" => LSymbol("nil")
        }
        return Some(res)
      }
      if (hold.size > 2) {
        LList(head, tail)
        temp = elements match {
          case ")" => LSymbol("nil")
          case "(" => LSymbol("nil")
          case _   => strToLObj(elements)

        }
        if (temp != LSymbol("nil")) {
          res = cons(temp, tail)
          tl = LList(res, tail)
          tail = res
          var test: Option[LObject] = Some(res);
          result = test
        }
      }

    }

    result
  }

  // for testing
  def lineToLObj(line: String): LObject = tokensToLObjs(matchPat(line)) match {
    case Some(s) => s
    case None    => error
  }

  def setValue(varName: String, value: LObject): Unit = { //using global variables to store the value it sets the input string to that value
    (strToLObj(varName), value) match {
      case (LSymbol(sym), LNumber(num)) => reser = num
      case (LSymbol(sym), LSymbol(t))   => gloStr = t

    }

    // TO DO: assign a value to a variable
  }

  def getValue(varName: String): LObject = {
    // TO DO: get the value of a variable; or error if variable not defined
    val hold = strToLObj(varName) match { //gets the value of the input string
      case LSymbol(sym) => LNumber(reser)
      case _            => error
    }
    hold
    //error
  }

  def add(a: LObject, b: LObject): LObject = { //takes input LObjects and matches them to the child LObjects so that the value can be stored and accessed
    // by a new LObject which will hold the sum of the a and b values
    var x = 0
    var y = 0
    a match {
      case LNumber(num: Int)    => x = num
      case LSymbol(sym: String) => return error
      case LList(head, tail)    => return error
    }

    b match {
      case LNumber(num: Int)    => y = num
      case LSymbol(sym: String) => return error
      case LList(head, tail)    => return error

    }

    val z: LObject = LNumber(x + y)

    z
  }

  // TO DO

  def sub(a: LObject, b: LObject): LObject = {
    var x = 0
    var y = 0
    a match {
      case LNumber(num: Int)    => x = num
      case LSymbol(sym: String) => return error
      case LList(head, tail)    => return error
    }

    b match {
      case LNumber(num: Int)    => y = num
      case LSymbol(sym: String) => return error
      case LList(head, tail)    => return error
    }

    val z: LObject = LNumber(x - y)
    z

  } // TO DO

  def mul(a: LObject, b: LObject): LObject = {
    var x = 0
    var y = 0
    a match {
      case LNumber(num: Int)    => x = num
      case LSymbol(sym: String) => return error
      case LList(head, tail)    => return error
    }

    b match {
      case LNumber(num: Int)    => y = num
      case LSymbol(sym: String) => return error
      case LList(head, tail)    => return error
    }

    val z: LObject = LNumber(x * y)
    z

  } // TO DO

  def div(a: LObject, b: LObject): LObject = {
    var x = 0
    var y = 0
    a match {
      case LNumber(num: Int)    => x = num
      case LSymbol(sym: String) => return error
      case LList(head, tail)    => return error
    }

    b match {
      case LNumber(num: Int)    => y = num
      case LSymbol(sym: String) => return error
      case LList(head, tail)    => return error
    }

    val z: LObject = LNumber(x / y)
    z

  } // TO DO

  def car(a: LObject): LObject = { //a is matched to a LList LObject so the head can be accessed
    a match {
      case LList(head, tail)    => head
      case LSymbol(sym: String) => return error
      case LNumber(num: Int)    => return error
    }
  } // TO DO

  def cdr(a: LObject): LObject = { //a is matched to a LList LObject so the tail can be accessed
    a match {
      case LList(head, tail)    => tail
      case LSymbol(sym: String) => return error
      case LNumber(num: Int)    => return error
    }
  } // TO DO

  def cons(a: LObject, b: LObject): LObject = { //a is matched to a child of LObject so that it can be put on the front of a LList
    a match {
      case LSymbol(sym)      => LList(LSymbol(sym), b)
      case LNumber(num)      => LList(LNumber(num), b)
      case LList(head, tail) => LList(LList(head, tail), b)
    }

  } // TO DO

  def eeqq(a: LObject, b: LObject): LObject = { //checks if a and b are equal
    if (a == b) {
      return T
    } else {
      return LSymbol("nil")
    }

  } // TO DO

  def setq(v: String, b: LObject): LObject = { //sets and gets the values for the inputs
    setValue(v, b)
    var hold = getValue(v)
    hold
  } // TO DO

  def iiff(cond: LObject, ifThen: LObject, ifElse: LObject): LObject = { // matches the cond to its appropriate case where eval is called upon either of its parameters to
    // return the result of that parameter
    val hold: LObject = cond match {
      case T              => eval(ifThen)
      case LSymbol("nil") => eval(ifElse)
      case _              => error
    }
    hold
  } //TO DO

  def defun(name: String, arg: String, body: LObject): LObject = {
    // TO DO: define a function
    // the function definition source would look like:
    //      (def name (arg) body)
    LSymbol(name)
  }

  def funCall(name: String, arg: LObject): LObject = error // TO DO

  def eval(a: LObject): LObject =
    a match { //evaluates the input a into its various LISP functions by matching it to the inputs appropriate case. In the case of extreme nested inputs
      // the eval function is recursed so the LList can broken down into its inidvidual bits and have the existing functions called to return its
      // desired result.
      case LSymbol("nil")    => nil
      case LSymbol("t")      => T
      case LNumber(num: Int) => LNumber(num: Int)
      case LSymbol(sym) =>
        getValue(sym) match {
          case LNumber(0) => LSymbol(sym)
          case _          => getValue(sym)
        }
      case LList(inithead, inittail) =>
        inithead match {

          case LSymbol(sym) =>
            sym match {
              case "car" =>
                inittail match {
                  case LList(head, tail) =>
                    head match {
                      case LSymbol(sym) => eval(car(inittail))
                      case _            => car(eval(head))
                    }

                }
              case "cdr" =>
                inittail match {
                  case LList(head, tail) =>
                    head match {
                      case LSymbol(sym) => eval(cdr(inittail))
                      case _            => cdr(eval(head))
                    }
                }
              case "eq" =>
                inittail match {
                  case LList(head, tail) =>
                    eeqq(head, tail)
                    tail match {
                      case LList(inhead, tail) => eeqq(eval(head), eval(inhead))
                    }
                }

              case "if" =>
                inittail match {
                  case LList(head, tail) =>
                    tail match {
                      case LList(inhead, intail) =>
                        intail match {
                          case LList(ininhead, inintail) =>
                            iiff(eval(head), eval(inhead), eval(ininhead))
                        }
                    }
                }
              case "cons" =>
                inittail match {
                  case LList(head, tail) =>
                    val conshold = head
                    tail match {
                      case LList(inhead, tail) => cons(eval(head), eval(inhead))
                    }
                }
              case "eval" =>
                inittail match {
                  case LList(head, tail) => eval(head)
                }

              case "setq" =>
                inittail match {
                  case LList(head, tail) =>
                    head match {
                      case LSymbol(sym) => setq(sym, eval(tail))

                    }
                }

              case "quote" =>
                inittail match {
                  case LList(LList(LSymbol("+"), s), tail) =>
                    eval(LList(LSymbol("+"), s))
                  case LList(LList(LSymbol("-"), s), tail) =>
                    eval(LList(LSymbol("-"), s))
                  case LList(LList(LSymbol("*"), s), tail) =>
                    eval(LList(LSymbol("*"), s))
                  case LList(LList(LSymbol("/"), s), tail) =>
                    eval(LList(LSymbol("/"), s))
                  case LList(head, tail) => head
                }

              case "+" =>
                inittail match {
                  case LList(xhead, xtail) =>
                    xhead match {
                      case LNumber(num) =>
                        val xhold = xhead
                        xtail match {
                          case LList(yhead, ytail) =>
                            yhead match {
                              case LNumber(num) => add(xhold, yhead)

                              case LList(head, tail) => add(xhead, eval(yhead))
                            }
                        }
                      case LList(head, tail) =>
                        val hhold = eval(xhead)
                        xtail match {
                          case LList(head, tail) => add(hhold, eval(head))
                        }

                    }

                }
              case "-" =>
                inittail match {
                  case LList(xhead, xtail) =>
                    xhead match {
                      case LNumber(num) =>
                        val xhold = xhead
                        xtail match {
                          case LList(yhead, ytail) =>
                            yhead match {
                              case LNumber(num) => sub(xhold, yhead)

                              case LList(head, tail) => sub(xhead, eval(yhead))

                            }

                        }
                      case LList(head, tail) =>
                        val hhold = eval(xhead)
                        xtail match {
                          case LList(head, tail) => sub(hhold, eval(head))
                        }

                    }
                }
              case "*" =>
                inittail match {
                  case LList(xhead, xtail) =>
                    xhead match {
                      case LNumber(num) =>
                        val xhold = xhead
                        xtail match {
                          case LList(yhead, ytail) =>
                            yhead match {
                              case LNumber(num)      => mul(xhold, yhead)
                              case LList(head, tail) => mul(xhead, eval(yhead))
                            }

                        }
                      case LList(head, tail) =>
                        val hhold = eval(xhead)
                        xtail match {
                          case LList(head, tail) => mul(hhold, eval(head))
                        }

                    }
                }
              case "/" =>
                inittail match {
                  case LList(xhead, xtail) =>
                    xhead match {
                      case LNumber(num) =>
                        val xhold = xhead
                        xtail match {
                          case LList(yhead, ytail) =>
                            yhead match {
                              case LNumber(num)      => div(xhold, yhead)
                              case LList(head, tail) => div(xhead, eval(yhead))
                            }

                        }
                      case LList(head, tail) =>
                        val hhold = eval(xhead)
                        xtail match {
                          case LList(head, tail) => div(hhold, eval(head))
                        }

                    }
                }
            }
          case LList(head, tail) => eval(head)
          case _                 => eval(inithead)
        }

      // TO DO: add cases for all the other possibilities in the eval table
      //        in the spec
      case _ => a
    }

  def showLine(s: LObject): Unit = { show(s); println() }

  def show(s: LObject): Unit = s match {
    case LList(h, t) =>
      print("(")
      show(h)
      showList(t)
      print(")")
    case LSymbol(a) => print(a)
    case LNumber(a) => print(a)
  }

  def showList(s: LObject): Unit = s match {
    case LSymbol("nil") =>
    case a: LList =>
      print(" ")
      show(a.head)
      a.tail match {
        case b: LList => showList(b)
        case _        =>
      }
    case _ =>
      print(" . ")
      show(s)
  }

}
