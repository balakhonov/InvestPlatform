package com.balakhonov.invest.dto

case class UpdateBotDto(canBuy: Option[Boolean],
                        canSell: Option[Boolean],
                        isActive: Option[Boolean])
