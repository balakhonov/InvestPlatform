package com.balakhonov.invest.util

object ColorHelper {
  def withRED(s: String) = s"${Color.RED}$s${Color.RESET}"

  def withGREEN(s: String) = s"${Color.GREEN}$s${Color.RESET}"

  def withWHITE(s: String) = s"${Color.WHITE}$s${Color.RESET}"

}
