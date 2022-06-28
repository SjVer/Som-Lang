"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var fs = require("fs");
var path = require("path");
var yaml = require("js-yaml");
var plist = require("plist");
// var Language;
// (function (Language) {
//     Language["TypeScript"] = "TypeScript";
//     Language["TypeScriptReact"] = "TypeScriptReact";
// })(Language || (Language = {}));
// var Extension;
// (function (Extension) {
//     Extension["TmLanguage"] = "tmLanguage";
//     Extension["TmTheme"] = "tmTheme";
//     Extension["YamlTmLanguage"] = "YAML-tmLanguage";
//     Extension["YamlTmTheme"] = "YAML-tmTheme";
// })(Extension || (Extension = {}));
// function file(language, extension) {
//     return path.join(__dirname, "..", "".concat(language, ".").concat(extension));
// }
function writePlistFile(grammar, fileName) {
    var text = plist.build(grammar);
    fs.writeFileSync(fileName, text);
}
function readYaml(fileName) {
    var text = fs.readFileSync(fileName, "utf8");
    return yaml.load(text);
}
// function changeTsToTsx(str) {
//     return str.replace(/\.ts/g, ".tsx");
// }
function transformGrammarRule(rule, propertyNames, transformProperty) {
    for (
        var _i = 0, propertyNames_1 = propertyNames;
        _i < propertyNames_1.length;
        _i++
    ) {
        var propertyName_1 = propertyNames_1[_i];
        var value = rule[propertyName_1];
        if (typeof value === "string") {
            rule[propertyName_1] = transformProperty(value);
        }
    }
    for (var propertyName in rule) {
        var value = rule[propertyName];
        if (typeof value === "object") {
            transformGrammarRule(value, propertyNames, transformProperty);
        }
    }
}
function transformGrammarRepository(grammar, propertyNames, transformProperty) {
    var repository = grammar.repository;
    for (var key in repository) {
        transformGrammarRule(repository[key], propertyNames, transformProperty);
    }
}
// function getTsxGrammar() {
//     var variables;
//     var tsxUpdatesBeforeTransformation = readYaml(
//         file(Language.TypeScriptReact, Extension.YamlTmLanguage)
//     );
//     var grammar = getTsGrammar(function (tsGrammarVariables) {
//         variables = tsGrammarVariables;
//         for (var variableName in tsxUpdatesBeforeTransformation.variables) {
//             variables[variableName] =
//                 tsxUpdatesBeforeTransformation.variables[variableName];
//         }
//         return variables;
//     });
//     var tsxUpdates = updateGrammarVariables(
//         tsxUpdatesBeforeTransformation,
//         variables
//     );
//     // Update name, file types, scope name and uuid
//     grammar.name = tsxUpdates.name;
//     grammar.scopeName = tsxUpdates.scopeName;
//     grammar.fileTypes = tsxUpdates.fileTypes;
//     grammar.uuid = tsxUpdates.uuid;
//     // Update scope names to .tsx
//     transformGrammarRepository(grammar, ["name", "contentName"], changeTsToTsx);
//     // Add repository items
//     var repository = grammar.repository;
//     var updatesRepository = tsxUpdates.repository;
//     for (var key in updatesRepository) {
//         switch (key) {
//             case "expressionWithoutIdentifiers":
//                 // Update expression
//                 repository[key].patterns.unshift(updatesRepository[key].patterns[0]);
//                 break;
//             default:
//                 // Add jsx
//                 repository[key] = updatesRepository[key];
//         }
//     }
//     return grammar;
// }
function getGrammar(getVariables) {
// function getTsGrammar(getVariables) {
    var grammarBeforeTransformation = readYaml(
        // file(Language.TypeScript, Extension.YamlTmLanguage)
        "./syntaxes/som.tmLanguage.yaml"
    );
    return updateGrammarVariables(
        grammarBeforeTransformation,
        getVariables(grammarBeforeTransformation.variables)
    );
}
function replacePatternVariables(pattern, variableReplacers) {
    var result = pattern;
    for (
        var _i = 0, variableReplacers_1 = variableReplacers;
        _i < variableReplacers_1.length;
        _i++
    ) {
        var _a = variableReplacers_1[_i],
            variableName = _a[0],
            value = _a[1];
        result = result.replace(variableName, value);
    }
    return result;
}
function updateGrammarVariables(grammar, variables) {
    delete grammar.variables;
    var variableReplacers = [];
    for (var variableName in variables) {
        // Replace the pattern with earlier variables
        var pattern = replacePatternVariables(
            variables[variableName],
            variableReplacers
        );
        variableReplacers.push([
            new RegExp("{{".concat(variableName, "}}"), "gim"),
            pattern,
        ]);
    }
    transformGrammarRepository(
        grammar,
        ["begin", "end", "match"],
        function (pattern) {
            return replacePatternVariables(pattern, variableReplacers);
        }
    );
    return grammar;
}
function buildGrammar() {
    var grammar = getGrammar(function (grammarVariables) {
        return grammarVariables;
    });
    // Write TypeScript.tmLanguage
    // writePlistFile(grammar, file(Language.TypeScript, Extension.TmLanguage));
    writePlistFile(grammar, "./syntaxes/som.generated.tmLanguage");
    // Write TypeScriptReact.tmLangauge
    // var tsxGrammar = getTsxGrammar();
    // writePlistFile(
    //     tsxGrammar,
    //     file(Language.TypeScriptReact, Extension.TmLanguage)
    // );
}
// function changeTsToTsxTheme(theme) {
//     var tsxUpdates = readYaml(
//         file(Language.TypeScriptReact, Extension.YamlTmTheme)
//     );
//     // Update name, uuid
//     theme.name = tsxUpdates.name;
//     theme.uuid = tsxUpdates.uuid;
//     // Update scope names to .tsx
//     var settings = theme.settings;
//     for (var i = 0; i < settings.length; i++) {
//         settings[i].scope = changeTsToTsx(settings[i].scope);
//     }
//     // Add additional setting items
//     theme.settings = theme.settings.concat(tsxUpdates.settings);
//     return theme;
// }
// function buildTheme() {
//     var tsTheme = readYaml(file(Language.TypeScript, Extension.YamlTmTheme));
//     // Write TypeScript.tmTheme
//     writePlistFile(tsTheme, file(Language.TypeScript, Extension.TmTheme));
//     // Write TypeScriptReact.thTheme
//     var tsxTheme = changeTsToTsxTheme(tsTheme);
//     writePlistFile(tsxTheme, file(Language.TypeScriptReact, Extension.TmTheme));
// }
buildGrammar();
// buildTheme();
