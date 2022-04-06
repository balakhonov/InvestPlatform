# --- !Ups

CREATE TABLE `bot`
(
    `id`                int(11) unsigned NOT NULL AUTO_INCREMENT,
    `figi`              varchar(12)      NOT NULL,
    `strategy`          varchar(45)      NOT NULL,
    `strategy_settings` mediumtext       NOT NULL,
    `is_active`         bit(1)           NOT NULL,
    `can_buy`           bit(1)           NOT NULL DEFAULT TRUE,
    `can_sell`          bit(1)           NOT NULL DEFAULT TRUE,
    PRIMARY KEY (`id`),
    KEY `bot_figi_idx` (`figi`),
    UNIQUE KEY `bot_uk` (`figi`, `strategy`)
)
    /*! ENGINE = InnoDB */
    /*! DEFAULT CHARACTER SET = utf8 */
    /*! COLLATE = utf8_general_ci */;

CREATE TABLE `instrument`
(
    `id`          int(11)          NOT NULL AUTO_INCREMENT,
    `provider_id` int(11) unsigned NOT NULL DEFAULT '0',
    `figi`        varchar(45)      NOT NULL,
    `name`        varchar(120)     NOT NULL,
    `ticker`      varchar(45)               DEFAULT NULL,
    `isin`        varchar(45)               DEFAULT NULL,
    `lot_size`    int(11)                   DEFAULT NULL,
    `currency`    varchar(3)                DEFAULT NULL,
    `type`        varchar(12)               DEFAULT NULL,
    PRIMARY KEY (`id`),
    UNIQUE KEY `instrument_figi_uk` (`figi`)
)
    /*! ENGINE = InnoDB */
    /*! DEFAULT CHARACTER SET = utf8 */
    /*! COLLATE = utf8_general_ci */;

CREATE TABLE `candle`
(
    `id`            int(11)                 NOT NULL AUTO_INCREMENT,
    `figi`          varchar(12)             NOT NULL,
    `date_time`     datetime                NOT NULL,
    `open_price`    decimal(15, 7) unsigned NOT NULL,
    `closing_price` decimal(15, 7) unsigned NOT NULL,
    `highest_price` decimal(15, 7) unsigned NOT NULL,
    `lowest_price`  decimal(15, 7) unsigned NOT NULL,
    `trading_value` int(11)    DEFAULT NULL,
    `interval`      int(11) DEFAULT NULL,
    PRIMARY KEY (`id`),
    KEY `candle_figi_idx` (`figi`),
    KEY `candle_interval_idx` (`interval`)
)
    /*! ENGINE = InnoDB */
    /*! DEFAULT CHARACTER SET = utf8 */
    /*! COLLATE = utf8_general_ci */;

CREATE TABLE `limit_order`
(
    `id`             int(11) unsigned NOT NULL AUTO_INCREMENT,
    `created`        datetime         NOT NULL DEFAULT CURRENT_TIMESTAMP(),
    `executed`       datetime                  DEFAULT NULL,
    `figi`           varchar(12)      NOT NULL,
    `conditions`     text             NOT NULL,
    `order_ref`      varchar(45)               DEFAULT NULL,
    `operation`      varchar(45)      NOT NULL,
    `requested_lots` int(11) unsigned NOT NULL,
    `price`          decimal(15, 7)   NOT NULL,
    `commission`     decimal(15, 7)            DEFAULT NULL,
    `status`         varchar(45)      NOT NULL,
    `executed_lots`  varchar(45)      NOT NULL,
    `reject_reason`  text,
    `message`        text,
    PRIMARY KEY (`id`)
)
    /*! ENGINE = InnoDB */
    /*! DEFAULT CHARACTER SET = utf8 */
    /*! COLLATE = utf8_general_ci */;

CREATE TABLE `lot`
(
    `id`                  int(11) unsigned NOT NULL AUTO_INCREMENT,
    `created`             datetime         NOT NULL DEFAULT CURRENT_TIMESTAMP(),
    `order_id`            int(11) unsigned NOT NULL,
    `figi`                varchar(12)      NOT NULL,
    `order_ref`           varchar(45)      NOT NULL,
    `price`               decimal(15, 7)   NOT NULL,
    `commission`          decimal(15, 7)            DEFAULT NULL,
    `order_sell_created`  datetime                  DEFAULT NULL,
    `order_sell_executed` datetime                  DEFAULT NULL,
    `order_sell_ref`      varchar(45)               DEFAULT NULL,
    `net_profit`          decimal(15, 7)            DEFAULT NULL,
    `is_sold`             bit(1)           NOT NULL,
    `is_marked_to_sell`   bit(1)           NOT NULL,
    PRIMARY KEY (`id`)
)
    /*! ENGINE = InnoDB */
    /*! DEFAULT CHARACTER SET = utf8 */
    /*! COLLATE = utf8_general_ci */;

# --- !Downs