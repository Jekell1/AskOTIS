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
            SELECT GRFILE.GR_ID,
                   GRFILE.GR_NAME,
                   GRFILE.GR_LEVEL,
                   GRFILE.GR_MEMBER_ID_1,
                   GRFILE.GR_MEMBER_ID_2,
                   GRFILE.GR_MEMBER_ID_3,
                   GRFILE.GR_MEMBER_ID_4,
                   GRFILE.GR_MEMBER_ID_5,
                   GRFILE.GR_MEMBER_ID_6,
                   GRFILE.GR_MEMBER_ID_7,
                   GRFILE.GR_MEMBER_ID_8,
                   GRFILE.GR_MEMBER_ID_9,
                   GRFILE.GR_MEMBER_ID_10,
                   GRFILE.GR_MEMBER_ID_11,
                   GRFILE.GR_MEMBER_ID_12,
                   GRFILE.GR_MEMBER_ID_13,
                   GRFILE.GR_MEMBER_ID_14,
                   GRFILE.GR_MEMBER_ID_15,
                   GRFILE.GR_MEMBER_ID_16,
                   GRFILE.GR_MEMBER_ID_17,
                   GRFILE.GR_MEMBER_ID_18,
                   GRFILE.GR_MEMBER_ID_19,
                   GRFILE.GR_MEMBER_ID_20,
                   GRFILE.GR_MEMBER_ID_21,
                   GRFILE.GR_MEMBER_ID_22,
                   GRFILE.GR_MEMBER_ID_23,
                   GRFILE.GR_MEMBER_ID_24,
                   GRFILE.GR_MEMBER_ID_25,
                   GRFILE.GR_MEMBER_ID_26,
                   GRFILE.GR_MEMBER_ID_27,
                   GRFILE.GR_MEMBER_ID_28,
                   GRFILE.GR_MEMBER_ID_29,
                   GRFILE.GR_MEMBER_ID_30,
                   GRFILE.GR_MEMBER_ID_31,
                   GRFILE.GR_MEMBER_ID_32,
                   GRFILE.GR_MEMBER_ID_33,
                   GRFILE.GR_MEMBER_ID_34,
                   GRFILE.GR_MEMBER_ID_35,
                   GRFILE.GR_MEMBER_ID_36,
                   GRFILE.GR_MEMBER_ID_37,
                   GRFILE.GR_MEMBER_ID_38,
                   GRFILE.GR_MEMBER_ID_39,
                   GRFILE.GR_MEMBER_ID_40,
                   GRFILE.GR_MEMBER_ID_41,
                   GRFILE.GR_MEMBER_ID_42,
                   GRFILE.GR_MEMBER_ID_43,
                   GRFILE.GR_MEMBER_ID_44,
                   GRFILE.GR_MEMBER_ID_45,
                   GRFILE.GR_MEMBER_ID_46,
                   GRFILE.GR_MEMBER_ID_47,
                   GRFILE.GR_MEMBER_ID_48,
                   GRFILE.GR_MEMBER_ID_49,
                   GRFILE.GR_MEMBER_ID_50,
                   GRFILE.GR_MEMBER_ID_51,
                   GRFILE.GR_MEMBER_ID_52,
                   GRFILE.GR_MEMBER_ID_53,
                   GRFILE.GR_MEMBER_ID_54,
                   GRFILE.GR_MEMBER_ID_55,
                   GRFILE.GR_MEMBER_ID_56,
                   GRFILE.GR_MEMBER_ID_57,
                   GRFILE.GR_MEMBER_ID_58,
                   GRFILE.GR_MEMBER_ID_59,
                   GRFILE.GR_MEMBER_ID_60,
                   GRFILE.GR_MEMBER_ID_61,
                   GRFILE.GR_MEMBER_ID_62,
                   GRFILE.GR_MEMBER_ID_63,
                   GRFILE.GR_MEMBER_ID_64,
                   GRFILE.GR_MEMBER_ID_65,
                   GRFILE.GR_MEMBER_ID_66,
                   GRFILE.GR_MEMBER_ID_67,
                   GRFILE.GR_MEMBER_ID_68,
                   GRFILE.GR_MEMBER_ID_69,
                   GRFILE.GR_MEMBER_ID_70,
                   GRFILE.GR_MEMBER_ID_71,
                   GRFILE.GR_MEMBER_ID_72,
                   GRFILE.GR_MEMBER_ID_73,
                   GRFILE.GR_MEMBER_ID_74,
                   GRFILE.GR_MEMBER_ID_75,
                   GRFILE.GR_MEMBER_ID_76,
                   GRFILE.GR_MEMBER_ID_77,
                   GRFILE.GR_MEMBER_ID_78,
                   GRFILE.GR_MEMBER_ID_79,
                   GRFILE.GR_MEMBER_ID_80,
                   GRFILE.GR_MEMBER_ID_81,
                   GRFILE.GR_MEMBER_ID_82,
                   GRFILE.GR_MEMBER_ID_83,
                   GRFILE.GR_MEMBER_ID_84,
                   GRFILE.GR_MEMBER_ID_85,
                   GRFILE.GR_MEMBER_ID_86,
                   GRFILE.GR_MEMBER_ID_87,
                   GRFILE.GR_MEMBER_ID_88,
                   GRFILE.GR_MEMBER_ID_89,
                   GRFILE.GR_MEMBER_ID_90,
                   GRFILE.GR_MEMBER_ID_91,
                   GRFILE.GR_MEMBER_ID_92,
                   GRFILE.GR_MEMBER_ID_93,
                   GRFILE.GR_MEMBER_ID_94,
                   GRFILE.GR_MEMBER_ID_95,
                   GRFILE.GR_MEMBER_ID_96,
                   GRFILE.GR_MEMBER_ID_97,
                   GRFILE.GR_MEMBER_ID_98,
                   GRFILE.GR_MEMBER_ID_99,
                   GRFILE.GR_MEMBER_ID_100,
                   GRFILE.GR_CONTACT,
                   GRFILE.GR_ADDR_1,
                   GRFILE.GR_ADDR_2,
                   GRFILE.GR_CITY,
                   GRFILE.GR_STATE,
                   GRFILE.GR_ZIP,
                   GRFILE.GR_PHONE,
                   GRFILE.GR_HOME_BRANCH,
                   GRFILE.GR_LN_APPROVAL_LIMIT,
                   CAST(GRFILE.GR_LTOUCH_DATE
                                                  AS VARCHAR(10))
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
           INITIALIZE QGR-REC.

           MOVE GR-ID TO QGR-ID.

           EXEC SQL
            SELECT GRFILE.GR_ID,
                   GRFILE.GR_NAME,
                   GRFILE.GR_LEVEL,
                   GRFILE.GR_MEMBER_ID_1,
                   GRFILE.GR_MEMBER_ID_2,
                   GRFILE.GR_MEMBER_ID_3,
                   GRFILE.GR_MEMBER_ID_4,
                   GRFILE.GR_MEMBER_ID_5,
                   GRFILE.GR_MEMBER_ID_6,
                   GRFILE.GR_MEMBER_ID_7,
                   GRFILE.GR_MEMBER_ID_8,
                   GRFILE.GR_MEMBER_ID_9,
                   GRFILE.GR_MEMBER_ID_10,
                   GRFILE.GR_MEMBER_ID_11,
                   GRFILE.GR_MEMBER_ID_12,
                   GRFILE.GR_MEMBER_ID_13,
                   GRFILE.GR_MEMBER_ID_14,
                   GRFILE.GR_MEMBER_ID_15,
                   GRFILE.GR_MEMBER_ID_16,
                   GRFILE.GR_MEMBER_ID_17,
                   GRFILE.GR_MEMBER_ID_18,
                   GRFILE.GR_MEMBER_ID_19,
                   GRFILE.GR_MEMBER_ID_20,
                   GRFILE.GR_MEMBER_ID_21,
                   GRFILE.GR_MEMBER_ID_22,
                   GRFILE.GR_MEMBER_ID_23,
                   GRFILE.GR_MEMBER_ID_24,
                   GRFILE.GR_MEMBER_ID_25,
                   GRFILE.GR_MEMBER_ID_26,
                   GRFILE.GR_MEMBER_ID_27,
                   GRFILE.GR_MEMBER_ID_28,
                   GRFILE.GR_MEMBER_ID_29,
                   GRFILE.GR_MEMBER_ID_30,
                   GRFILE.GR_MEMBER_ID_31,
                   GRFILE.GR_MEMBER_ID_32,
                   GRFILE.GR_MEMBER_ID_33,
                   GRFILE.GR_MEMBER_ID_34,
                   GRFILE.GR_MEMBER_ID_35,
                   GRFILE.GR_MEMBER_ID_36,
                   GRFILE.GR_MEMBER_ID_37,
                   GRFILE.GR_MEMBER_ID_38,
                   GRFILE.GR_MEMBER_ID_39,
                   GRFILE.GR_MEMBER_ID_40,
                   GRFILE.GR_MEMBER_ID_41,
                   GRFILE.GR_MEMBER_ID_42,
                   GRFILE.GR_MEMBER_ID_43,
                   GRFILE.GR_MEMBER_ID_44,
                   GRFILE.GR_MEMBER_ID_45,
                   GRFILE.GR_MEMBER_ID_46,
                   GRFILE.GR_MEMBER_ID_47,
                   GRFILE.GR_MEMBER_ID_48,
                   GRFILE.GR_MEMBER_ID_49,
                   GRFILE.GR_MEMBER_ID_50,
                   GRFILE.GR_MEMBER_ID_51,
                   GRFILE.GR_MEMBER_ID_52,
                   GRFILE.GR_MEMBER_ID_53,
                   GRFILE.GR_MEMBER_ID_54,
                   GRFILE.GR_MEMBER_ID_55,
                   GRFILE.GR_MEMBER_ID_56,
                   GRFILE.GR_MEMBER_ID_57,
                   GRFILE.GR_MEMBER_ID_58,
                   GRFILE.GR_MEMBER_ID_59,
                   GRFILE.GR_MEMBER_ID_60,
                   GRFILE.GR_MEMBER_ID_61,
                   GRFILE.GR_MEMBER_ID_62,
                   GRFILE.GR_MEMBER_ID_63,
                   GRFILE.GR_MEMBER_ID_64,
                   GRFILE.GR_MEMBER_ID_65,
                   GRFILE.GR_MEMBER_ID_66,
                   GRFILE.GR_MEMBER_ID_67,
                   GRFILE.GR_MEMBER_ID_68,
                   GRFILE.GR_MEMBER_ID_69,
                   GRFILE.GR_MEMBER_ID_70,
                   GRFILE.GR_MEMBER_ID_71,
                   GRFILE.GR_MEMBER_ID_72,
                   GRFILE.GR_MEMBER_ID_73,
                   GRFILE.GR_MEMBER_ID_74,
                   GRFILE.GR_MEMBER_ID_75,
                   GRFILE.GR_MEMBER_ID_76,
                   GRFILE.GR_MEMBER_ID_77,
                   GRFILE.GR_MEMBER_ID_78,
                   GRFILE.GR_MEMBER_ID_79,
                   GRFILE.GR_MEMBER_ID_80,
                   GRFILE.GR_MEMBER_ID_81,
                   GRFILE.GR_MEMBER_ID_82,
                   GRFILE.GR_MEMBER_ID_83,
                   GRFILE.GR_MEMBER_ID_84,
                   GRFILE.GR_MEMBER_ID_85,
                   GRFILE.GR_MEMBER_ID_86,
                   GRFILE.GR_MEMBER_ID_87,
                   GRFILE.GR_MEMBER_ID_88,
                   GRFILE.GR_MEMBER_ID_89,
                   GRFILE.GR_MEMBER_ID_90,
                   GRFILE.GR_MEMBER_ID_91,
                   GRFILE.GR_MEMBER_ID_92,
                   GRFILE.GR_MEMBER_ID_93,
                   GRFILE.GR_MEMBER_ID_94,
                   GRFILE.GR_MEMBER_ID_95,
                   GRFILE.GR_MEMBER_ID_96,
                   GRFILE.GR_MEMBER_ID_97,
                   GRFILE.GR_MEMBER_ID_98,
                   GRFILE.GR_MEMBER_ID_99,
                   GRFILE.GR_MEMBER_ID_100,
                   GRFILE.GR_CONTACT,
                   GRFILE.GR_ADDR_1,
                   GRFILE.GR_ADDR_2,
                   GRFILE.GR_CITY,
                   GRFILE.GR_STATE,
                   GRFILE.GR_ZIP,
                   GRFILE.GR_PHONE,
                   GRFILE.GR_HOME_BRANCH,
                   GRFILE.GR_LN_APPROVAL_LIMIT,
                   CAST(GRFILE.GR_LTOUCH_DATE
                                                  AS VARCHAR(10))
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
           PERFORM CLOSE-IT.
           IF ( GR1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GR1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GR1-CURSOR-STAT.
 
