package org.updraft0.controltower.protocol

// used with the ESI auth callback
case class CodeAndState(code: String, state: String)

case class SessionCookie(value: String)
