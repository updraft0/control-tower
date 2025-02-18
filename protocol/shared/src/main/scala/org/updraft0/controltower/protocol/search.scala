package org.updraft0.controltower.protocol

enum SearchType derives CanEqual:
  case Any, Character, Alliance, Corporation

enum SearchEntityResponse:
  case OfCharacter(value: UserCharacter)
  case OfAlliance(value: Alliance)
  case OfCorporation(value: Corporation)
