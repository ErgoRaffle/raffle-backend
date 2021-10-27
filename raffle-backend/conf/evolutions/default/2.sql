-- schema

-- !Ups
CREATE TABLE RAFFLES(
    "ID"            BIGINT NOT NULL PRIMARY KEY AUTO_INCREMENT,
    "NAME"          VARCHAR(255) NOT NULL,
    "DESCRIPTION"   VARCHAR(10000),
    GOAL            BIGINT NOT NULL,
    RAISED          BIGINT NOT NULL,
    DEADLINE_HEIGHT BIGINT NOT NULL,
    SERVICE_FEE     INT NOT NULL,
    CHARITY_PERCENT INT NOT NULL,
    CHARITY_ADD     VARCHAR(10000) NOT NULL,
    TICKET_PRICE    BIGINT NOT NULL,
    PIC_LINKS       VARCHAR(10000),
    TICKETS         BIGINT,
    PARTICIPANTS    BIGINT,
    REDEEM_TICKETS  BIGINT,
    "STATE"         INT,
    RAFFLE_TOKEN    VARCHAR(1000),
    CREATION_TIME   LONG,
    LAST_ACTIVITY   LONG,
    IS_UPDATING     BOOLEAN,
    COMPLETED       BOOLEAN
);

CREATE TABLE TRANSACTIONS(
    "ID"        BIGINT NOT NULL PRIMARY KEY AUTO_INCREMENT,
    TX_ID       VARCHAR(100),
    TOKEN_ID    VARCHAR(100),
    TOKEN_COUNT BIGINT,
    "TYPE"      VARCHAR(100),
    WALLET_ADD  VARCHAR(1000),
    CONSTRAINT TX_TOKEN UNIQUE (TX_ID, TOKEN_ID)
);
-- !Downs

