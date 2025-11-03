      * COPYMEMBER: LIBGB/GBGR1RN
       LOAD-GR1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GR-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GR-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO GR-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GR-ALL-MACHINE
              MOVE GR-ALL-PATH             TO GR-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
      *- - - -{ DO NOT KNOW WHERE THIS WOULD BE USED, }- - - - - - - - -
      *- - - -{ BUT STAYING CONSISTENT  }- - - - - - - - - - - - - - - -
           IF ( GR-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO GR-ALL-BASE
              MOVE FILPATH-MACHINE         TO GR-ALL-MACHINE
              MOVE GR-ALL-PATH             TO GR-PATH.

      *-----------------------------------------------------------------
       OPEN-GR1-FILE.
           PERFORM LOAD-GR1-FILE.
           PERFORM OPEN-IT.
           MOVE GR-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GR1-FILE.
           PERFORM START-IT.
           MOVE GR-PATH-SQL TO E-FILE.
           MOVE GR1-KEY TO E-KEYX.

           IF ( GR1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GR1-FILE.

           IF ( QGR1-WEND-ID = SPACES    )
              MOVE GR-ID                TO QGR1-WBEG-ID
              MOVE ALL "Z"              TO QGR1-WEND-ID
              CALL "C$TOLOWER" USING QGR1-WEND-ID, VALUE 4.

           EXEC SQL
            DECLARE GR1_CURSOR CURSOR FOR
            SELECT ISNULL(GRFILE.GR_ID,''),
                   ISNULL(GRFILE.GR_NAME,''),
                   GRFILE.GR_LEVEL,
                   ISNULL(GRFILE.GR_MEMBER_ID_1,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_2,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_3,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_4,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_5,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_6,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_7,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_8,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_9,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_10,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_11,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_12,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_13,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_14,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_15,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_16,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_17,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_18,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_19,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_20,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_21,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_22,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_23,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_24,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_25,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_26,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_27,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_28,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_29,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_30,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_31,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_32,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_33,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_34,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_35,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_36,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_37,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_38,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_39,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_40,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_41,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_42,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_43,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_44,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_45,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_46,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_47,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_48,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_49,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_50,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_51,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_52,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_53,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_54,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_55,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_56,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_57,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_58,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_59,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_60,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_61,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_62,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_63,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_64,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_65,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_66,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_67,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_68,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_69,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_70,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_71,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_72,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_73,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_74,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_75,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_76,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_77,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_78,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_79,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_80,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_81,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_82,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_83,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_84,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_85,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_86,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_87,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_88,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_89,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_90,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_91,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_92,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_93,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_94,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_95,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_96,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_97,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_98,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_99,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_100,''),
                   ISNULL(GRFILE.GR_CONTACT,''),
                   ISNULL(GRFILE.GR_ADDR_1,''),
                   ISNULL(GRFILE.GR_ADDR_2,''),
                   ISNULL(GRFILE.GR_CITY,''),
                   ISNULL(GRFILE.GR_STATE,''),
                   ISNULL(GRFILE.GR_ZIP,''),
                   GRFILE.GR_PHONE,
                   GRFILE.GR_HOME_BRANCH,
                   GRFILE.GR_LN_APPROVAL_LIMIT,
                   ISNULL(CAST(GRFILE.GR_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0')
            FROM DBO.GRFILE
            WHERE GRFILE.GR_ID >= :QGR1-WBEG-ID
              AND GRFILE.GR_ID <= :QGR1-WEND-ID
            ORDER BY GRFILE.GR_ID
           END-EXEC.

           EXEC SQL
               OPEN GR1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGR1-WBEG-ID
                          QGR1-WEND-ID.

           MOVE STAT---GOOD TO GR1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GR1-FILE.
           PERFORM READ-IT.
           MOVE GR-PATH-SQL TO E-FILE.
           MOVE GR1-KEY TO E-KEYX.

           MOVE GR-ID TO QGR-ID.

           EXEC SQL
            SELECT ISNULL(GRFILE.GR_ID,''),
                   ISNULL(GRFILE.GR_NAME,''),
                   GRFILE.GR_LEVEL,
                   ISNULL(GRFILE.GR_MEMBER_ID_1,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_2,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_3,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_4,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_5,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_6,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_7,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_8,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_9,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_10,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_11,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_12,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_13,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_14,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_15,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_16,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_17,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_18,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_19,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_20,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_21,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_22,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_23,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_24,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_25,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_26,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_27,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_28,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_29,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_30,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_31,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_32,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_33,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_34,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_35,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_36,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_37,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_38,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_39,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_40,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_41,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_42,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_43,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_44,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_45,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_46,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_47,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_48,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_49,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_50,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_51,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_52,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_53,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_54,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_55,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_56,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_57,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_58,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_59,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_60,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_61,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_62,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_63,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_64,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_65,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_66,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_67,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_68,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_69,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_70,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_71,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_72,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_73,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_74,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_75,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_76,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_77,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_78,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_79,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_80,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_81,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_82,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_83,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_84,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_85,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_86,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_87,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_88,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_89,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_90,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_91,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_92,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_93,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_94,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_95,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_96,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_97,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_98,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_99,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_100,''),
                   ISNULL(GRFILE.GR_CONTACT,''),
                   ISNULL(GRFILE.GR_ADDR_1,''),
                   ISNULL(GRFILE.GR_ADDR_2,''),
                   ISNULL(GRFILE.GR_CITY,''),
                   ISNULL(GRFILE.GR_STATE,''),
                   ISNULL(GRFILE.GR_ZIP,''),
                   GRFILE.GR_PHONE,
                   GRFILE.GR_HOME_BRANCH,
                   GRFILE.GR_LN_APPROVAL_LIMIT,
                   ISNULL(CAST(GRFILE.GR_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0')
            INTO :QGR-REC
            FROM DBO.GRFILE
            WHERE GRFILE.GR_ID = :QGR-ID
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-GR1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GR-FIELDS.

      *-----------------------------------------------------------------
       READ-GR1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GR-PATH-SQL TO E-FILE.
           MOVE GR1-KEY TO E-KEYX.
           INITIALIZE QGR-REC.

           IF ( GR1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GR1_CURSOR INTO :QGR-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-GR1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GR-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GR1-FILE.
           IF ( GR1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GR1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GR1-CURSOR-STAT.
 
