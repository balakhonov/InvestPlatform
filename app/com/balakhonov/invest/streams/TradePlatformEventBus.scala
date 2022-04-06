package com.balakhonov.invest.streams

import akka.actor.typed.ActorRef
import akka.event.{EventBus, LookupClassification}
import com.balakhonov.invest.streams.TradePlatformEventBus.PlatformClassifier

import javax.inject.{Inject, Singleton}


@Singleton
case class TradePlatformEventBus @Inject()()
  extends EventBus
    with LookupClassification {

  override type Event = TradePlatformEventBus.Event
  override type Classifier = String
  override type Subscriber = ActorRef[TradePlatformEventBus.Event]

  // is used for extracting the classifier from the incoming events
  override protected def classify(event: Event): Classifier = {
    event match {
      case event: TradePlatformEventBus.InstrumentEvent => event.figi
      case _ => PlatformClassifier
    }
  }

  // will be invoked for each event for all subscribers which registered themselves
  // for the event’s classifier
  override protected def publish(event: Event, subscriber: Subscriber): Unit = {
    subscriber ! event
  }

  // must define a full order over the subscribers, expressed as expected from
  // `java.lang.Comparable.compare`
  override protected def compareSubscribers(a: Subscriber, b: Subscriber): Int =
    a.compareTo(b)

  // determines the initial size of the index data structure
  // used internally (i.e. the expected number of different classifiers)
  override protected def mapSize(): Int = 128

}

object TradePlatformEventBus {
  val PlatformClassifier = "platform"

  trait Event

  trait InstrumentEvent extends Event {
    def figi: String
  }

}