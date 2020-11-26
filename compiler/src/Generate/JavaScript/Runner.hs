{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Generate.JavaScript.Runner
  ( runner
  )
  where


import qualified Data.ByteString.Builder as B
import Text.RawString.QQ (r)



-- RUNNER


runner :: B.Builder
runner = [r|

successes = []
errors = []

for (let moduleName in generators) {
  for (let declarationName in generators[moduleName]) {
    let io = generators[moduleName][declarationName]
    let result = performIo(io)
    if (result.$ == "Done")
      successes.push({ moduleName, declarationName, v: result.a })
    else
      errors.push({ moduleName, declarationName, v: result.a })
  }
}

if (errors.length !== 0) {
  console.log(JSON.stringify({
    type: "errors",
    errors: errors
  }, null, 1))
  process.exit(0)
}

let elmData = {}
for (let { moduleName, declarationName, v } of successes) {
  if (!elmData[moduleName])
    elmData[moduleName] = {}

  if (declarationName.startsWith("_"))
    declarationName = declarationName.slice(1)

  elmData[moduleName][declarationName] = toElmValue(v)
}

console.log(JSON.stringify({
  type: "success",
  results: elmData
}, null, 1))
process.exit(0)


function performIo(io) {
  if (io.$ === "Fail")
    return io

  if (io.$ === "Done")
    return io

  if (io.$ === "IO") {
    let command = io.a
    let args = toArray(io.b)
    let decoder = io.c

    let commandResult = performCommand(command, args)
    let decodeResult = _Json_runHelp(decoder, commandResult)

    if (decodeResult.$ === "Err")
      throw new Error("Decode error")

    return performIo(decodeResult.a)
  }

  throw new Error("Invalid IO Variant: `" + io.$ + "`")
}

function performCommand(command, args) {
  if (command === "Environment.getVariable") {
    let [ name ] = args
    name = name.a

    let value = process.env[name]
    if (typeof value === "undefined")
      return null
    else
      return value
  }

  throw new Error("Invalid IO command: `" + command + "`")
}

function toElmValue(v) {
  if (typeof v === "boolean") {
    if (v) return "True"
    else return "False"
  }

  if (typeof v === "number")
    return "" + v

  if (v instanceof String)
    return "'" + v + "'"

  if (typeof v === "string")
    return "\"" + v + "\""

  if (typeof v === "object" && "$" in v) {
    let tag = v.$

    if (tag === "#0")
      return "()"

    if (tag[0] === "#") {
      let children = []
      for (k in v) {
        if (k === "$") continue
        children.push(toElmValue(v[k]))
      }
      return "( " + children.join(", ") + " )"
    }

    if (tag === "Set_elm_builtin") {
      let asList = $elm$core$Set$toList(v)
      return "Set.fromList " + toElmValue(asList)
    }

    if (tag === "RBNode_elm_builtin" || tag === "RBEmpty_elm_builtin") {
      let asList = $elm$core$Dict$toList(v)
      return "Dict.fromList " + toElmValue(asList)
    }

    if (tag === "Array_elm_builtin") {
      let asList = $elm$core$Array$toList(v)
      return "Array.fromList " + toElmValue(asList)
    }

    if (tag === "::" || tag === "[]") {
      let array = toArray(v)
      array = "[" + array.map(toElmValue).join(", ") + "]"
      return array
    }

    let children = [ v.a, v.b, v.c, v.d, v.e, v.f, v.g, v.h ]
      .filter(c => typeof c !== "undefined")
      .map(toElmValue)
      .join(" ")

    return tag + " " + children
  }

  if (typeof v === "object" && ! ("$" in v) ) {
    keyValues = []
    for (let key in v) {
      let value = v[key]
      keyValues.push([key, toElmValue(value)])
    }

    let singleLine = "{ "
      + keyValues.map(([k, v]) => k + " = " + v).join(", ")
      + " }"

    return singleLine
  }

  console.log(v)
}

function toArray(elmList) {
  if (elmList.$ === "::")
    return [elmList.a, ...toArray(elmList.b)]

  if (elmList.$ == "[]")
    return []
}

function indentMultiline(n, s) {
  if (isMultiline(s)) {
    let i = new Array(n).fill(" ").join("")
    return i + s.split("\n").join("\n" + i)
  } else {
    return s
  }
}

function isMultiline(s) {
  return s.indexOf("\n") > -1
}

|]
