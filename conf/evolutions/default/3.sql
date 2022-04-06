# --- !Ups

ALTER TABLE `bot`
    ADD COLUMN `lots_to_buy` INT(11) UNSIGNED NOT NULL DEFAULT 1;

# --- !Downs