#!/usr/bin/env node
// vi: ft=javascript

const readline = require("readline");

// ANSI escape codes for colors
const Colors = {
  RESET: "\x1b[0m",
  RED: "\x1b[31m",
  YELLOW: "\x1b[33m",
  CYAN: "\x1b[36m",
  BOLD: "\x1b[1m",
  UNDERLINE: "\x1b[4m",
};

function colorForLevel(level) {
  switch (level.toUpperCase()) {
    case "INFO":
      return Colors.CYAN;
    case "WARN":
    case "WARNING":
      return Colors.YELLOW;
    case "ERR":
    case "ERROR":
      return Colors.RED;
    default:
      return Colors.RESET;
  }
}

/**
 * Builds a formatted message string from an object's fields.
 * @param {object | Array} data - The object or array to process.
 * @param {string[]} [excludeList=[]] - List of keys to exclude from the output.
 * @returns {string} The formatted message string.
 */
function buildFieldMessage(data, excludeList = []) {
  if (!data) {
    return "";
  }

  const lines = [];
  for (const key in data) {
    const value = data[key];
    if (value) {
      if (excludeList.includes(key)) {
        continue;
      }

      // Check if it's a non-null, non-array object and not empty
      if (
        typeof value === "object" &&
        value !== null &&
        !Array.isArray(value)
      ) {
        lines.push(`${key} = {`);
        for (const nestedKey in value) {
          lines.push(`\t${nestedKey} = ${value[nestedKey]}`);
        }
        lines.push("}");
      } else {
        lines.push(`${key} = ${value}`);
      }
    }
  }

  return lines.length > 0 ? "\n\t" + lines.join("\n\t") : "";
}

/**
 * Extracts the log level from the data object.
 * @param {object} data - The log data object.
 * @returns {string} The detected log level, or "INFO" if not found.
 */
function getLevel(data) {
  for (const key in data) {
    if (typeof key === "string" && key.toLowerCase().includes("level")) {
      return data[key];
    }
  }

  return "INFO";
}

/**
 * Extracts the timestamp from the data object.
 * @param {object} data - The log data object.
 * @returns {{field: string, value: string}} An object containing the field name and timestamp value.
 */
function getTimestamp(data) {
  const timeFields = ["time", "timestamp"];
  for (const field of timeFields) {
    const value = data[field];
    if (value !== undefined) {
      return { field, value };
    }
  }

  const context = data.context || {};
  if (context.asctime !== undefined) {
    return { field: "asctime", value: context.asctime };
  }

  // Default to current ISO timestamp if no timestamp found
  return { field: "", value: new Date().toISOString() };
}

/**
 * Pretty prints a single JSON log line.
 * @param {string} line - The JSON string representing a log entry.
 */
function prettyPrint(line) {
  const data = JSON.parse(line);
  const { field: timestampField, value: timestamp } = getTimestamp(data);
  let msgContent = data.message || data.msg || "";
  const level = getLevel(data);

  // Format the header part of the message
  let formattedMsg = `${colorForLevel(level)}${level}${Colors.RESET} ${timestamp} ${msgContent}`;

  // Fields to exclude from the main data object when building the field message
  const excludeList = [
    "message",
    "msg",
    timestampField,
    "level",
    "levelname",
    "level_name",
    "context",
    "asctime", // Exclude as it's handled by timestamp logic
  ];

  formattedMsg += buildFieldMessage(data, excludeList);
  formattedMsg += buildFieldMessage(data.context || {});

  console.log(formattedMsg);
}

/**
 * Main function to read input and pretty print log lines.
 * This conversion assumes the script primarily reads from standard input,
 * similar to `fileinput.input()` when no file arguments are provided in Python.
 * For handling file arguments in Node.js, `fs.createReadStream` for each file
 * would be used, which is a more complex implementation than simple stdin.
 */
function main() {
  // Check if there are file arguments.
  // For this conversion, we prioritize reading from stdin as typical for piped logs.
  // If explicit file processing is needed, the logic here would expand.
  const fileArgs = process.argv.slice(2);

  if (fileArgs.length > 0) {
    // This is a basic handling for file arguments for demonstration.
    // A more robust solution would handle large files with streams.
    for (const filePath of fileArgs) {
      try {
        const fileContent = require("fs").readFileSync(filePath, "utf8");
        fileContent.split("\n").forEach((fileLine) => {
          if (fileLine.trim() !== "") {
            try {
              prettyPrint(fileLine);
            } catch (e) {
              if (e instanceof SyntaxError) {
                console.log("NON-JSON LINE (from file):", fileLine);
              } else {
                console.error("Error processing file line:", fileLine, e);
              }
            }
          }
        });
      } catch (err) {
        console.error(`Error reading file ${filePath}:`, err.message);
      }
    }
    // If files are processed, we assume stdin should not be read simultaneously
    return;
  }

  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    terminal: false,
  });

  rl.on("line", (line) => {
    try {
      prettyPrint(line);
    } catch (e) {
      if (e instanceof SyntaxError) {
        // JSON.parse throws SyntaxError for invalid JSON
        console.log("NON-JSON LINE:", line);
      } else {
        console.error("Error processing line:", line, e);
      }
    }
  });
}

// Execute the main function
main();
