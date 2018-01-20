package io

import java.io.File
import java.nio.charset.StandardCharsets
import toplev.Pass
import utils.FileUtils

/* This pass reads the file into a string.  */

object Input extends Pass[File, InputString]("input") {
  def run(file: File) = {
    new InputString(FileUtils.readFileToString(file, StandardCharsets.UTF_8))
  }
}
