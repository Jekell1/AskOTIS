      * COPYMEMBER: LIBLP/LPIRRN
       LOAD-IR-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( IR-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( IR-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO IR-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO IR-ALL-MACHINE
              MOVE IR-ALL-PATH             TO IR-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( IR-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO IR-ALL-BASE
              MOVE FILPATH-MACHINE         TO IR-ALL-MACHINE
              MOVE IR-ALL-PATH             TO IR-PATH.

      *-----------------------------------------------------------------
       OPEN-IR-FILE.
           PERFORM LOAD-IR-FILE.
           PERFORM OPEN-IT.
           MOVE IR-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-IR-FILE.
           PERFORM START-IT.
           MOVE IR-PATH-SQL TO E-FILE.
           MOVE IR-KEY TO E-KEYX.

           IF ( IR-CURSOR-STAT-GOOD )
              PERFORM CLOSE-IR-FILE.

           IF ( QIR-WEND-KEY = ZEROES    ) OR
              ( QIR-WEND-KEY NOT NUMERIC )
              MOVE IR-KEY               TO QIR-WBEG-KEY
              MOVE 999                  TO QIR-WEND-KEY.

           EXEC SQL
            DECLARE IR_CURSOR CURSOR FOR
            SELECT IRFILE.IR_KEY,
                   CAST(IRFILE.IR_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   IRFILE.IR_PER_FG,
                   IRFILE.IR_INSRATE_1,
                   IRFILE.IR_INSRATE_2,
                   IRFILE.IR_INSRATE_3,
                   IRFILE.IR_INSRATE_4,
                   IRFILE.IR_INSRATE_5,
                   IRFILE.IR_INSRATE_6,
                   IRFILE.IR_INSRATE_7,
                   IRFILE.IR_INSRATE_8,
                   IRFILE.IR_INSRATE_9,
                   IRFILE.IR_INSRATE_10,
                   IRFILE.IR_INSRATE_11,
                   IRFILE.IR_INSRATE_12,
                   IRFILE.IR_INSRATE_13,
                   IRFILE.IR_INSRATE_14,
                   IRFILE.IR_INSRATE_15,
                   IRFILE.IR_INSRATE_16,
                   IRFILE.IR_INSRATE_17,
                   IRFILE.IR_INSRATE_18,
                   IRFILE.IR_INSRATE_19,
                   IRFILE.IR_INSRATE_20,
                   IRFILE.IR_INSRATE_21,
                   IRFILE.IR_INSRATE_22,
                   IRFILE.IR_INSRATE_23,
                   IRFILE.IR_INSRATE_24,
                   IRFILE.IR_INSRATE_25,
                   IRFILE.IR_INSRATE_26,
                   IRFILE.IR_INSRATE_27,
                   IRFILE.IR_INSRATE_28,
                   IRFILE.IR_INSRATE_29,
                   IRFILE.IR_INSRATE_30,
                   IRFILE.IR_INSRATE_31,
                   IRFILE.IR_INSRATE_32,
                   IRFILE.IR_INSRATE_33,
                   IRFILE.IR_INSRATE_34,
                   IRFILE.IR_INSRATE_35,
                   IRFILE.IR_INSRATE_36,
                   IRFILE.IR_INSRATE_37,
                   IRFILE.IR_INSRATE_38,
                   IRFILE.IR_INSRATE_39,
                   IRFILE.IR_INSRATE_40,
                   IRFILE.IR_INSRATE_41,
                   IRFILE.IR_INSRATE_42,
                   IRFILE.IR_INSRATE_43,
                   IRFILE.IR_INSRATE_44,
                   IRFILE.IR_INSRATE_45,
                   IRFILE.IR_INSRATE_46,
                   IRFILE.IR_INSRATE_47,
                   IRFILE.IR_INSRATE_48,
                   IRFILE.IR_INSRATE_49,
                   IRFILE.IR_INSRATE_50,
                   IRFILE.IR_INSRATE_51,
                   IRFILE.IR_INSRATE_52,
                   IRFILE.IR_INSRATE_53,
                   IRFILE.IR_INSRATE_54,
                   IRFILE.IR_INSRATE_55,
                   IRFILE.IR_INSRATE_56,
                   IRFILE.IR_INSRATE_57,
                   IRFILE.IR_INSRATE_58,
                   IRFILE.IR_INSRATE_59,
                   IRFILE.IR_INSRATE_60,
                   IRFILE.IR_INSRATE_61,
                   IRFILE.IR_INSRATE_62,
                   IRFILE.IR_INSRATE_63,
                   IRFILE.IR_INSRATE_64,
                   IRFILE.IR_INSRATE_65,
                   IRFILE.IR_INSRATE_66,
                   IRFILE.IR_INSRATE_67,
                   IRFILE.IR_INSRATE_68,
                   IRFILE.IR_INSRATE_69,
                   IRFILE.IR_INSRATE_70,
                   IRFILE.IR_INSRATE_71,
                   IRFILE.IR_INSRATE_72,
                   IRFILE.IR_INSRATE_73,
                   IRFILE.IR_INSRATE_74,
                   IRFILE.IR_INSRATE_75,
                   IRFILE.IR_INSRATE_76,
                   IRFILE.IR_INSRATE_77,
                   IRFILE.IR_INSRATE_78,
                   IRFILE.IR_INSRATE_79,
                   IRFILE.IR_INSRATE_80,
                   IRFILE.IR_INSRATE_81,
                   IRFILE.IR_INSRATE_82,
                   IRFILE.IR_INSRATE_83,
                   IRFILE.IR_INSRATE_84,
                   IRFILE.IR_INSRATE_85,
                   IRFILE.IR_INSRATE_86,
                   IRFILE.IR_INSRATE_87,
                   IRFILE.IR_INSRATE_88,
                   IRFILE.IR_INSRATE_89,
                   IRFILE.IR_INSRATE_90,
                   IRFILE.IR_INSRATE_91,
                   IRFILE.IR_INSRATE_92,
                   IRFILE.IR_INSRATE_93,
                   IRFILE.IR_INSRATE_94,
                   IRFILE.IR_INSRATE_95,
                   IRFILE.IR_INSRATE_96,
                   IRFILE.IR_INSRATE_97,
                   IRFILE.IR_INSRATE_98,
                   IRFILE.IR_INSRATE_99,
                   IRFILE.IR_INSRATE_100,
                   IRFILE.IR_INSRATE_101,
                   IRFILE.IR_INSRATE_102,
                   IRFILE.IR_INSRATE_103,
                   IRFILE.IR_INSRATE_104,
                   IRFILE.IR_INSRATE_105,
                   IRFILE.IR_INSRATE_106,
                   IRFILE.IR_INSRATE_107,
                   IRFILE.IR_INSRATE_108,
                   IRFILE.IR_INSRATE_109,
                   IRFILE.IR_INSRATE_110,
                   IRFILE.IR_INSRATE_111,
                   IRFILE.IR_INSRATE_112,
                   IRFILE.IR_INSRATE_113,
                   IRFILE.IR_INSRATE_114,
                   IRFILE.IR_INSRATE_115,
                   IRFILE.IR_INSRATE_116,
                   IRFILE.IR_INSRATE_117,
                   IRFILE.IR_INSRATE_118,
                   IRFILE.IR_INSRATE_119,
                   IRFILE.IR_INSRATE_120,
                   IRFILE.IR_DESC
            FROM DBO.IRFILE
            WHERE IRFILE.IR_KEY >= :QIR-WBEG-KEY
              AND IRFILE.IR_KEY <= :QIR-WEND-KEY
            ORDER BY IRFILE.IR_KEY
           END-EXEC.

           EXEC SQL
               OPEN IR_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QIR-WBEG-KEY
                          QIR-WEND-KEY.

           MOVE STAT---GOOD TO IR-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-IR-FILE.
           PERFORM READ-IT.
           MOVE IR-PATH-SQL TO E-FILE.
           MOVE IR-KEY TO E-KEYX.
           INITIALIZE QIR-REC.

           MOVE IR-KEY    TO QIR-KEY.  

           EXEC SQL
            SELECT IRFILE.IR_KEY,
                   CAST(IRFILE.IR_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   IRFILE.IR_PER_FG,
                   IRFILE.IR_INSRATE_1,
                   IRFILE.IR_INSRATE_2,
                   IRFILE.IR_INSRATE_3,
                   IRFILE.IR_INSRATE_4,
                   IRFILE.IR_INSRATE_5,
                   IRFILE.IR_INSRATE_6,
                   IRFILE.IR_INSRATE_7,
                   IRFILE.IR_INSRATE_8,
                   IRFILE.IR_INSRATE_9,
                   IRFILE.IR_INSRATE_10,
                   IRFILE.IR_INSRATE_11,
                   IRFILE.IR_INSRATE_12,
                   IRFILE.IR_INSRATE_13,
                   IRFILE.IR_INSRATE_14,
                   IRFILE.IR_INSRATE_15,
                   IRFILE.IR_INSRATE_16,
                   IRFILE.IR_INSRATE_17,
                   IRFILE.IR_INSRATE_18,
                   IRFILE.IR_INSRATE_19,
                   IRFILE.IR_INSRATE_20,
                   IRFILE.IR_INSRATE_21,
                   IRFILE.IR_INSRATE_22,
                   IRFILE.IR_INSRATE_23,
                   IRFILE.IR_INSRATE_24,
                   IRFILE.IR_INSRATE_25,
                   IRFILE.IR_INSRATE_26,
                   IRFILE.IR_INSRATE_27,
                   IRFILE.IR_INSRATE_28,
                   IRFILE.IR_INSRATE_29,
                   IRFILE.IR_INSRATE_30,
                   IRFILE.IR_INSRATE_31,
                   IRFILE.IR_INSRATE_32,
                   IRFILE.IR_INSRATE_33,
                   IRFILE.IR_INSRATE_34,
                   IRFILE.IR_INSRATE_35,
                   IRFILE.IR_INSRATE_36,
                   IRFILE.IR_INSRATE_37,
                   IRFILE.IR_INSRATE_38,
                   IRFILE.IR_INSRATE_39,
                   IRFILE.IR_INSRATE_40,
                   IRFILE.IR_INSRATE_41,
                   IRFILE.IR_INSRATE_42,
                   IRFILE.IR_INSRATE_43,
                   IRFILE.IR_INSRATE_44,
                   IRFILE.IR_INSRATE_45,
                   IRFILE.IR_INSRATE_46,
                   IRFILE.IR_INSRATE_47,
                   IRFILE.IR_INSRATE_48,
                   IRFILE.IR_INSRATE_49,
                   IRFILE.IR_INSRATE_50,
                   IRFILE.IR_INSRATE_51,
                   IRFILE.IR_INSRATE_52,
                   IRFILE.IR_INSRATE_53,
                   IRFILE.IR_INSRATE_54,
                   IRFILE.IR_INSRATE_55,
                   IRFILE.IR_INSRATE_56,
                   IRFILE.IR_INSRATE_57,
                   IRFILE.IR_INSRATE_58,
                   IRFILE.IR_INSRATE_59,
                   IRFILE.IR_INSRATE_60,
                   IRFILE.IR_INSRATE_61,
                   IRFILE.IR_INSRATE_62,
                   IRFILE.IR_INSRATE_63,
                   IRFILE.IR_INSRATE_64,
                   IRFILE.IR_INSRATE_65,
                   IRFILE.IR_INSRATE_66,
                   IRFILE.IR_INSRATE_67,
                   IRFILE.IR_INSRATE_68,
                   IRFILE.IR_INSRATE_69,
                   IRFILE.IR_INSRATE_70,
                   IRFILE.IR_INSRATE_71,
                   IRFILE.IR_INSRATE_72,
                   IRFILE.IR_INSRATE_73,
                   IRFILE.IR_INSRATE_74,
                   IRFILE.IR_INSRATE_75,
                   IRFILE.IR_INSRATE_76,
                   IRFILE.IR_INSRATE_77,
                   IRFILE.IR_INSRATE_78,
                   IRFILE.IR_INSRATE_79,
                   IRFILE.IR_INSRATE_80,
                   IRFILE.IR_INSRATE_81,
                   IRFILE.IR_INSRATE_82,
                   IRFILE.IR_INSRATE_83,
                   IRFILE.IR_INSRATE_84,
                   IRFILE.IR_INSRATE_85,
                   IRFILE.IR_INSRATE_86,
                   IRFILE.IR_INSRATE_87,
                   IRFILE.IR_INSRATE_88,
                   IRFILE.IR_INSRATE_89,
                   IRFILE.IR_INSRATE_90,
                   IRFILE.IR_INSRATE_91,
                   IRFILE.IR_INSRATE_92,
                   IRFILE.IR_INSRATE_93,
                   IRFILE.IR_INSRATE_94,
                   IRFILE.IR_INSRATE_95,
                   IRFILE.IR_INSRATE_96,
                   IRFILE.IR_INSRATE_97,
                   IRFILE.IR_INSRATE_98,
                   IRFILE.IR_INSRATE_99,
                   IRFILE.IR_INSRATE_100,
                   IRFILE.IR_INSRATE_101,
                   IRFILE.IR_INSRATE_102,
                   IRFILE.IR_INSRATE_103,
                   IRFILE.IR_INSRATE_104,
                   IRFILE.IR_INSRATE_105,
                   IRFILE.IR_INSRATE_106,
                   IRFILE.IR_INSRATE_107,
                   IRFILE.IR_INSRATE_108,
                   IRFILE.IR_INSRATE_109,
                   IRFILE.IR_INSRATE_110,
                   IRFILE.IR_INSRATE_111,
                   IRFILE.IR_INSRATE_112,
                   IRFILE.IR_INSRATE_113,
                   IRFILE.IR_INSRATE_114,
                   IRFILE.IR_INSRATE_115,
                   IRFILE.IR_INSRATE_116,
                   IRFILE.IR_INSRATE_117,
                   IRFILE.IR_INSRATE_118,
                   IRFILE.IR_INSRATE_119,
                   IRFILE.IR_INSRATE_120,
                   IRFILE.IR_DESC
            INTO :QIR-REC
            FROM DBO.IRFILE
            WHERE IRFILE.IR_KEY = :QIR-KEY
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-IR-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-IR-FIELDS.

      *-----------------------------------------------------------------
       READ-IR-FILE-NEXT.
           PERFORM READ-IT.
           MOVE IR-PATH-SQL TO E-FILE.
           MOVE IR-KEY TO E-KEYX.
           INITIALIZE QIR-REC.

           IF ( IR-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH IR_CURSOR INTO :QIR-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-IR-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-IR-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-IR-FILE.
           PERFORM CLOSE-IT.
           IF ( IR-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE IR_CURSOR
              END-EXEC
              MOVE STAT---BAD TO IR-CURSOR-STAT.

