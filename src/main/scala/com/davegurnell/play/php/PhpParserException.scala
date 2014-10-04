package com.davegurnell.play.php

/**
 * Exception thrown by the `PhpParser` when it encounters unknown input.
 */
case class PhpParserException(message: String) extends Exception(message)