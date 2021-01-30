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

var fs = require("fs")

successes = []
errors = []

for (let moduleName in generators) {
  for (let declarationName in generators[moduleName]) {
    let task = generators[moduleName][declarationName]
    let result = performTask(task)
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

  elmData[moduleName][declarationName] = "\n    " + toElmValue(v, 4)
}

console.log(JSON.stringify({
  type: "success",
  results: elmData
}, null, 1))
process.exit(0)


function performTask(task) {
  if (task.$ === "Fail")
    return task

  if (task.$ === "Done")
    return task

  if (task.$ === "Task") {
    let command = task.a
    let args = toArray(task.b)
    let decoder = task.c

    let commandResult = performCommand(command, args)
    let decodeResult = _Json_runHelp(decoder, commandResult)

    if (decodeResult.$ === "Err")
      throw new Error("Decode error")

    return performTask(decodeResult.a)
  }

  throw new Error("Invalid Task Variant: `" + task.$ + "`")
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

  if (command === "Embed.File.read_") {
    let [ path ] = args
    path = path.a

    try {
      let contents = fs.readFileSync(path, "utf8")
      contents = contents
        .replace(/\\/g, "\\\\")
        .replace(/\n/g, "\\n")
        .replace(/"/g, '\\"')
      return { Ok : contents }
    } catch(e) {
      return { Err : e.message }
    }
  }

  throw new Error("Invalid IO command: `" + command + "`")
}

function toElmValue(v, indent) {
  let p = (" ").repeat(indent)
  if (typeof v === "boolean") {
    if (v) return "True"
    else return "False"
  }

  if (typeof v === "number")
    return v

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
        children.push(toElmValue(v[k], indent))
      }
      return "(" + children.join("\n" + p +  ", ") + "\n" + p + ")"
    }

    if (tag === "Set_elm_builtin") {
      let asList = $elm$core$Set$toList(v)
      return "Set.fromList\n" + p + "    " + toElmValue(asList, indent + 4)
    }

    if (tag === "RBNode_elm_builtin" || tag === "RBEmpty_elm_builtin") {
      let asList = $elm$core$Dict$toList(v)
      return "Dict.fromList\n" + p + "    " + toElmValue(asList, indent + 4)
    }

    if (tag === "Array_elm_builtin") {
      let asList = $elm$core$Array$toList(v)
      return "Array.fromList\n" + p + "    " + toElmValue(asList, indent + 4)
    }

    if (tag === "::" || tag === "[]") {
      let array = toArray(v)
      array = "[ " + array.map(e => toElmValue(e, indent + 2)).join("\n" + p + ", ") + "\n" + p + "]"
      return array
    }

    let children = [ v.a, v.b, v.c, v.d, v.e, v.f, v.g, v.h ]
      .filter(c => typeof c !== "undefined")
      .map(e => {
        if (ambiguousSpacing(e)) {
          return "(" + toElmValue(e, indent + 4) + "\n" + p + "    )"
        }
        else {
          return toElmValue(e, indent + 4)
        }
      })
      .join("\n    " + p)

    return tag + "\n    " + p  + children
  }

  if (typeof v === "object" && ! ("$" in v) ) {
    keyValues = []
    for (let key in v) {
      let value = v[key]
      keyValues.push(key + " = " + toElmValue(value, indent + 2))
    }

    return "{ " + keyValues.map(s => s).join("\n" + p + ", ") + "\n" + p + "}"
  }

  console.log(v)
}

function ambiguousSpacing(v) {
  if ( typeof v === "object" && "$" in v && (v.$[0] !== "#" && v.$ !== "::"  && v.$ !== "[]") )
    return true
  else
    return false
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
