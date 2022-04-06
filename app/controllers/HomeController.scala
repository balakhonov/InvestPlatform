package controllers

import com.balakhonov.invest.dto.UpdateBotDto
import com.balakhonov.invest.services.PlatformManagerService
import controllers.util.JsonParametersParser._
import play.api.libs.json.JsValue
import play.api.mvc._

import javax.inject._
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(cc: ControllerComponents,
                               platformManagerService: PlatformManagerService)
  extends AbstractController(cc) {

  /**
   * Create an Action to render an HTML page with a welcome message.
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  def index: Action[AnyContent] = Action.async {
    val future = platformManagerService.getPlatformStatus

    future.map { dto =>
      Ok(dto.toJson)
    }
  }

  final def updateBot(id: Int): Action[JsValue] = Action(parse.json) { req =>
    val dto = parseUpdateBotDto()(req.body)

    platformManagerService.updateBot(id, dto)

    Ok
  }

  private def parseUpdateBotDto()(implicit body: JsValue): UpdateBotDto = {
    val canBuy = parseOptionalBoolean("canBuy")
    val canSell = parseOptionalBoolean("canSell")
    val isActive = parseOptionalBoolean("isActive")

    UpdateBotDto(
      canBuy = canBuy,
      canSell = canSell,
      isActive = isActive
    )
  }

}
