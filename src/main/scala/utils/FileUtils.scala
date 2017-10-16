package utils

import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.Path
import java.io.{File, PrintWriter}

object FileUtils {
  def readFileToString(file: File, encoding: Charset) = {
    val encoded = Files.readAllBytes(file.toPath)

    new String(encoded, encoding)
  }

  def writeStringToFile(filename: String, contents: String) = {
    val printWriter = new PrintWriter(filename)
    printWriter.println(contents)
    printWriter.close()
  }
}
