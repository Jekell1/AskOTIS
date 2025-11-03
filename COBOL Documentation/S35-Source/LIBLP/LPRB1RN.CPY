      * COPYMEMBER: LIBLP/LPRB1RN
       LOAD-RB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( RB-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( RB-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO RB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO RB-ALL-MACHINE
              MOVE RB-ALL-PATH             TO RB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( RB-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO RB-ALL-BASE
              MOVE FILPATH-MACHINE         TO RB-ALL-MACHINE
              MOVE RB-ALL-PATH             TO RB-PATH.

      *-----------------------------------------------------------------
       OPEN-RB1-FILE.
           PERFORM LOAD-RB1-FILE.
           PERFORM OPEN-IT.
           MOVE RB-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-RB1-FILE.
           PERFORM START-IT.
           MOVE RB-PATH-SQL TO E-FILE.
           MOVE RB1-KEY TO E-KEYX.

           IF ( RB1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-RB1-FILE.

           IF ( QRB1-WEND-TBL-NO = ZEROES    ) OR
              ( QRB1-WEND-TBL-NO NOT NUMERIC )
              MOVE RB-TBL-NO    TO QRB1-WBEG-TBL-NO
              MOVE 99           TO QRB1-WEND-TBL-NO
              MOVE RB-TERM      TO QRB1-WBEG-TERM
              MOVE 999          TO QRB1-WEND-TERM.

           EXEC SQL
            DECLARE RB1_CURSOR CURSOR FOR
            SELECT RBFILE.RB_TBL_NO,
                   RBFILE.RB_TERM,
                   RBFILE.RB_MON_DAY_CD,
                   RBFILE.RB_AMT_1,
                   RBFILE.RB_RATE_1,
                   RBFILE.RB_AMT_2,
                   RBFILE.RB_RATE_2,
                   RBFILE.RB_AMT_3,
                   RBFILE.RB_RATE_3,
                   RBFILE.RB_AMT_4,
                   RBFILE.RB_RATE_4,
                   RBFILE.RB_AMT_5,
                   RBFILE.RB_RATE_5,
                   RBFILE.RB_AMT_6,
                   RBFILE.RB_RATE_6,
                   RBFILE.RB_AMT_7,
                   RBFILE.RB_RATE_7,
                   RBFILE.RB_AMT_8,
                   RBFILE.RB_RATE_8,
                   RBFILE.RB_AMT_9,
                   RBFILE.RB_RATE_9,
                   RBFILE.RB_AMT_10,
                   RBFILE.RB_RATE_10,
                   RBFILE.RB_AMT_11,
                   RBFILE.RB_RATE_11,
                   RBFILE.RB_AMT_12,
                   RBFILE.RB_RATE_12,
                   RBFILE.RB_AMT_13,
                   RBFILE.RB_RATE_13,
                   RBFILE.RB_AMT_14,
                   RBFILE.RB_RATE_14,
                   RBFILE.RB_AMT_15,
                   RBFILE.RB_RATE_15,
                   RBFILE.RB_AMT_16,
                   RBFILE.RB_RATE_16,
                   RBFILE.RB_AMT_17,
                   RBFILE.RB_RATE_17,
                   RBFILE.RB_AMT_18,
                   RBFILE.RB_RATE_18,
                   RBFILE.RB_AMT_19,
                   RBFILE.RB_RATE_19,
                   RBFILE.RB_AMT_20,
                   RBFILE.RB_RATE_20,
                   RBFILE.RB_AMT_21,
                   RBFILE.RB_RATE_21,
                   RBFILE.RB_AMT_22,
                   RBFILE.RB_RATE_22,
                   RBFILE.RB_AMT_23,
                   RBFILE.RB_RATE_23,
                   RBFILE.RB_AMT_24,
                   RBFILE.RB_RATE_24,
                   RBFILE.RB_AMT_25,
                   RBFILE.RB_RATE_25,
                   RBFILE.RB_AMT_26,
                   RBFILE.RB_RATE_26,
                   RBFILE.RB_AMT_27,
                   RBFILE.RB_RATE_27,
                   RBFILE.RB_AMT_28,
                   RBFILE.RB_RATE_28,
                   RBFILE.RB_AMT_29,
                   RBFILE.RB_RATE_29,
                   RBFILE.RB_AMT_30,
                   RBFILE.RB_RATE_30,
                   RBFILE.RB_AMT_31,
                   RBFILE.RB_RATE_31,
                   RBFILE.RB_AMT_32,
                   RBFILE.RB_RATE_32,
                   RBFILE.RB_AMT_33,
                   RBFILE.RB_RATE_33,
                   RBFILE.RB_AMT_34,
                   RBFILE.RB_RATE_34,
                   RBFILE.RB_AMT_35,
                   RBFILE.RB_RATE_35,
                   RBFILE.RB_AMT_36,
                   RBFILE.RB_RATE_36,
                   RBFILE.RB_AMT_37,
                   RBFILE.RB_RATE_37,
                   RBFILE.RB_AMT_38,
                   RBFILE.RB_RATE_38,
                   RBFILE.RB_AMT_39,
                   RBFILE.RB_RATE_39,
                   RBFILE.RB_AMT_40,
                   RBFILE.RB_RATE_40,
                   RBFILE.RB_AMT_41,
                   RBFILE.RB_RATE_41,
                   RBFILE.RB_AMT_42,
                   RBFILE.RB_RATE_42,
                   RBFILE.RB_AMT_43,
                   RBFILE.RB_RATE_43,
                   RBFILE.RB_AMT_44,
                   RBFILE.RB_RATE_44,
                   RBFILE.RB_AMT_45,
                   RBFILE.RB_RATE_45,
                   RBFILE.RB_AMT_46,
                   RBFILE.RB_RATE_46,
                   RBFILE.RB_AMT_47,
                   RBFILE.RB_RATE_47,
                   RBFILE.RB_AMT_48,
                   RBFILE.RB_RATE_48,
                   RBFILE.RB_AMT_49,
                   RBFILE.RB_RATE_49,
                   RBFILE.RB_AMT_50,
                   RBFILE.RB_RATE_50,
                   RBFILE.RB_AMT_51,
                   RBFILE.RB_RATE_51,
                   RBFILE.RB_AMT_52,
                   RBFILE.RB_RATE_52,
                   RBFILE.RB_AMT_53,
                   RBFILE.RB_RATE_53,
                   RBFILE.RB_AMT_54,
                   RBFILE.RB_RATE_54,
                   RBFILE.RB_AMT_55,
                   RBFILE.RB_RATE_55,
                   RBFILE.RB_AMT_56,
                   RBFILE.RB_RATE_56,
                   RBFILE.RB_AMT_57,
                   RBFILE.RB_RATE_57,
                   RBFILE.RB_AMT_58,
                   RBFILE.RB_RATE_58,
                   RBFILE.RB_AMT_59,
                   RBFILE.RB_RATE_59,
                   RBFILE.RB_AMT_60,
                   RBFILE.RB_RATE_60,
                   RBFILE.RB_AMT_61,
                   RBFILE.RB_RATE_61,
                   RBFILE.RB_AMT_62,
                   RBFILE.RB_RATE_62,
                   RBFILE.RB_AMT_63,
                   RBFILE.RB_RATE_63,
                   RBFILE.RB_AMT_64,
                   RBFILE.RB_RATE_64,
                   RBFILE.RB_AMT_65,
                   RBFILE.RB_RATE_65,
                   RBFILE.RB_AMT_66,
                   RBFILE.RB_RATE_66,
                   RBFILE.RB_AMT_67,
                   RBFILE.RB_RATE_67,
                   RBFILE.RB_AMT_68,
                   RBFILE.RB_RATE_68,
                   RBFILE.RB_AMT_69,
                   RBFILE.RB_RATE_69,
                   RBFILE.RB_AMT_70,
                   RBFILE.RB_RATE_70,
                   RBFILE.RB_AMT_71,
                   RBFILE.RB_RATE_71,
                   RBFILE.RB_AMT_72,
                   RBFILE.RB_RATE_72,
                   RBFILE.RB_AMT_73,
                   RBFILE.RB_RATE_73,
                   RBFILE.RB_AMT_74,
                   RBFILE.RB_RATE_74,
                   RBFILE.RB_AMT_75,
                   RBFILE.RB_RATE_75,
                   CAST(RBFILE.RB_LTOUCH_DATE
                                                  AS VARCHAR(10))
            FROM DBO.RBFILE
            WHERE RBFILE.RB_TBL_NO >= :QRB1-WBEG-TBL-NO
              AND RBFILE.RB_TBL_NO <= :QRB1-WEND-TBL-NO
              AND RBFILE.RB_TERM >= :QRB1-WBEG-TERM
              AND RBFILE.RB_TERM <= :QRB1-WEND-TERM
            ORDER BY RBFILE.RB_TBL_NO,
                     RBFILE.RB_TERM
           END-EXEC.

           EXEC SQL
               OPEN RB1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QRB1-WBEG-TBL-NO
                          QRB1-WEND-TBL-NO
                          QRB1-WBEG-TERM
                          QRB1-WEND-TERM.

           MOVE STAT---GOOD TO RB1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-RB1-FILE.
           PERFORM READ-IT.
           MOVE RB-PATH-SQL TO E-FILE.
           MOVE RB1-KEY TO E-KEYX.
           INITIALIZE QRB-REC.  

           MOVE RB-TBL-NO    TO QRB-TBL-NO. 
           MOVE RB-TERM      TO QRB-TERM.

           EXEC SQL
            SELECT RBFILE.RB_TBL_NO,
                   RBFILE.RB_TERM,
                   RBFILE.RB_MON_DAY_CD,
                   RBFILE.RB_AMT_1,
                   RBFILE.RB_RATE_1,
                   RBFILE.RB_AMT_2,
                   RBFILE.RB_RATE_2,
                   RBFILE.RB_AMT_3,
                   RBFILE.RB_RATE_3,
                   RBFILE.RB_AMT_4,
                   RBFILE.RB_RATE_4,
                   RBFILE.RB_AMT_5,
                   RBFILE.RB_RATE_5,
                   RBFILE.RB_AMT_6,
                   RBFILE.RB_RATE_6,
                   RBFILE.RB_AMT_7,
                   RBFILE.RB_RATE_7,
                   RBFILE.RB_AMT_8,
                   RBFILE.RB_RATE_8,
                   RBFILE.RB_AMT_9,
                   RBFILE.RB_RATE_9,
                   RBFILE.RB_AMT_10,
                   RBFILE.RB_RATE_10,
                   RBFILE.RB_AMT_11,
                   RBFILE.RB_RATE_11,
                   RBFILE.RB_AMT_12,
                   RBFILE.RB_RATE_12,
                   RBFILE.RB_AMT_13,
                   RBFILE.RB_RATE_13,
                   RBFILE.RB_AMT_14,
                   RBFILE.RB_RATE_14,
                   RBFILE.RB_AMT_15,
                   RBFILE.RB_RATE_15,
                   RBFILE.RB_AMT_16,
                   RBFILE.RB_RATE_16,
                   RBFILE.RB_AMT_17,
                   RBFILE.RB_RATE_17,
                   RBFILE.RB_AMT_18,
                   RBFILE.RB_RATE_18,
                   RBFILE.RB_AMT_19,
                   RBFILE.RB_RATE_19,
                   RBFILE.RB_AMT_20,
                   RBFILE.RB_RATE_20,
                   RBFILE.RB_AMT_21,
                   RBFILE.RB_RATE_21,
                   RBFILE.RB_AMT_22,
                   RBFILE.RB_RATE_22,
                   RBFILE.RB_AMT_23,
                   RBFILE.RB_RATE_23,
                   RBFILE.RB_AMT_24,
                   RBFILE.RB_RATE_24,
                   RBFILE.RB_AMT_25,
                   RBFILE.RB_RATE_25,
                   RBFILE.RB_AMT_26,
                   RBFILE.RB_RATE_26,
                   RBFILE.RB_AMT_27,
                   RBFILE.RB_RATE_27,
                   RBFILE.RB_AMT_28,
                   RBFILE.RB_RATE_28,
                   RBFILE.RB_AMT_29,
                   RBFILE.RB_RATE_29,
                   RBFILE.RB_AMT_30,
                   RBFILE.RB_RATE_30,
                   RBFILE.RB_AMT_31,
                   RBFILE.RB_RATE_31,
                   RBFILE.RB_AMT_32,
                   RBFILE.RB_RATE_32,
                   RBFILE.RB_AMT_33,
                   RBFILE.RB_RATE_33,
                   RBFILE.RB_AMT_34,
                   RBFILE.RB_RATE_34,
                   RBFILE.RB_AMT_35,
                   RBFILE.RB_RATE_35,
                   RBFILE.RB_AMT_36,
                   RBFILE.RB_RATE_36,
                   RBFILE.RB_AMT_37,
                   RBFILE.RB_RATE_37,
                   RBFILE.RB_AMT_38,
                   RBFILE.RB_RATE_38,
                   RBFILE.RB_AMT_39,
                   RBFILE.RB_RATE_39,
                   RBFILE.RB_AMT_40,
                   RBFILE.RB_RATE_40,
                   RBFILE.RB_AMT_41,
                   RBFILE.RB_RATE_41,
                   RBFILE.RB_AMT_42,
                   RBFILE.RB_RATE_42,
                   RBFILE.RB_AMT_43,
                   RBFILE.RB_RATE_43,
                   RBFILE.RB_AMT_44,
                   RBFILE.RB_RATE_44,
                   RBFILE.RB_AMT_45,
                   RBFILE.RB_RATE_45,
                   RBFILE.RB_AMT_46,
                   RBFILE.RB_RATE_46,
                   RBFILE.RB_AMT_47,
                   RBFILE.RB_RATE_47,
                   RBFILE.RB_AMT_48,
                   RBFILE.RB_RATE_48,
                   RBFILE.RB_AMT_49,
                   RBFILE.RB_RATE_49,
                   RBFILE.RB_AMT_50,
                   RBFILE.RB_RATE_50,
                   RBFILE.RB_AMT_51,
                   RBFILE.RB_RATE_51,
                   RBFILE.RB_AMT_52,
                   RBFILE.RB_RATE_52,
                   RBFILE.RB_AMT_53,
                   RBFILE.RB_RATE_53,
                   RBFILE.RB_AMT_54,
                   RBFILE.RB_RATE_54,
                   RBFILE.RB_AMT_55,
                   RBFILE.RB_RATE_55,
                   RBFILE.RB_AMT_56,
                   RBFILE.RB_RATE_56,
                   RBFILE.RB_AMT_57,
                   RBFILE.RB_RATE_57,
                   RBFILE.RB_AMT_58,
                   RBFILE.RB_RATE_58,
                   RBFILE.RB_AMT_59,
                   RBFILE.RB_RATE_59,
                   RBFILE.RB_AMT_60,
                   RBFILE.RB_RATE_60,
                   RBFILE.RB_AMT_61,
                   RBFILE.RB_RATE_61,
                   RBFILE.RB_AMT_62,
                   RBFILE.RB_RATE_62,
                   RBFILE.RB_AMT_63,
                   RBFILE.RB_RATE_63,
                   RBFILE.RB_AMT_64,
                   RBFILE.RB_RATE_64,
                   RBFILE.RB_AMT_65,
                   RBFILE.RB_RATE_65,
                   RBFILE.RB_AMT_66,
                   RBFILE.RB_RATE_66,
                   RBFILE.RB_AMT_67,
                   RBFILE.RB_RATE_67,
                   RBFILE.RB_AMT_68,
                   RBFILE.RB_RATE_68,
                   RBFILE.RB_AMT_69,
                   RBFILE.RB_RATE_69,
                   RBFILE.RB_AMT_70,
                   RBFILE.RB_RATE_70,
                   RBFILE.RB_AMT_71,
                   RBFILE.RB_RATE_71,
                   RBFILE.RB_AMT_72,
                   RBFILE.RB_RATE_72,
                   RBFILE.RB_AMT_73,
                   RBFILE.RB_RATE_73,
                   RBFILE.RB_AMT_74,
                   RBFILE.RB_RATE_74,
                   RBFILE.RB_AMT_75,
                   RBFILE.RB_RATE_75,
                   CAST(RBFILE.RB_LTOUCH_DATE
                                                  AS VARCHAR(10))
            INTO :QRB-REC
            FROM DBO.RBFILE
            WHERE RBFILE.RB_TBL_NO = :QRB-TBL-NO
              AND RBFILE.RB_TERM = :QRB-TERM
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RB1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-RB-FIELDS.

      *-----------------------------------------------------------------
       READ-RB1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE RB-PATH-SQL TO E-FILE.
           MOVE RB1-KEY TO E-KEYX.
           INITIALIZE QRB-REC.

           IF ( RB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH RB1_CURSOR INTO :QRB-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RB1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-RB-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-RB1-FILE.
           PERFORM CLOSE-IT.
           IF ( RB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE RB1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO RB1-CURSOR-STAT.

