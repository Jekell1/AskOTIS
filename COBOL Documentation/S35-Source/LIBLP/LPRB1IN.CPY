      * COPYMEMBER: LIBLP/LPRB1IN
      *  BAH 2024.0610 FIXED TYPE-O RBFILLE.RB IN DELETE PARAGRAPH S35-64
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

       OPEN-RB1-FILE.
           PERFORM LOAD-RB1-FILE.
           PERFORM OPEN-IT.
           MOVE RB-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

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
       WRITE-RB1-FILE.

           ACCEPT RB-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE RB-PATH-SQL TO E-FILE.
           MOVE RB1-KEY TO E-KEYX.
           PERFORM SET-RB-FIELDS.

           EXEC SQL
            INSERT INTO DBO.RBFILE
             (RBFILE.RB_TBL_NO,
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
              RBFILE.RB_LTOUCH_DATE)
            VALUES
             (:QRB-TBL-NO,
              :QRB-TERM,
              :QRB-MON-DAY-CD,
              :QRB-AMT-1,
              :QRB-RATE-1,
              :QRB-AMT-2,
              :QRB-RATE-2,
              :QRB-AMT-3,
              :QRB-RATE-3,
              :QRB-AMT-4,
              :QRB-RATE-4,
              :QRB-AMT-5,
              :QRB-RATE-5,
              :QRB-AMT-6,
              :QRB-RATE-6,
              :QRB-AMT-7,
              :QRB-RATE-7,
              :QRB-AMT-8,
              :QRB-RATE-8,
              :QRB-AMT-9,
              :QRB-RATE-9,
              :QRB-AMT-10,
              :QRB-RATE-10,
              :QRB-AMT-11,
              :QRB-RATE-11,
              :QRB-AMT-12,
              :QRB-RATE-12,
              :QRB-AMT-13,
              :QRB-RATE-13,
              :QRB-AMT-14,
              :QRB-RATE-14,
              :QRB-AMT-15,
              :QRB-RATE-15,
              :QRB-AMT-16,
              :QRB-RATE-16,
              :QRB-AMT-17,
              :QRB-RATE-17,
              :QRB-AMT-18,
              :QRB-RATE-18,
              :QRB-AMT-19,
              :QRB-RATE-19,
              :QRB-AMT-20,
              :QRB-RATE-20,
              :QRB-AMT-21,
              :QRB-RATE-21,
              :QRB-AMT-22,
              :QRB-RATE-22,
              :QRB-AMT-23,
              :QRB-RATE-23,
              :QRB-AMT-24,
              :QRB-RATE-24,
              :QRB-AMT-25,
              :QRB-RATE-25,
              :QRB-AMT-26,
              :QRB-RATE-26,
              :QRB-AMT-27,
              :QRB-RATE-27,
              :QRB-AMT-28,
              :QRB-RATE-28,
              :QRB-AMT-29,
              :QRB-RATE-29,
              :QRB-AMT-30,
              :QRB-RATE-30,
              :QRB-AMT-31,
              :QRB-RATE-31,
              :QRB-AMT-32,
              :QRB-RATE-32,
              :QRB-AMT-33,
              :QRB-RATE-33,
              :QRB-AMT-34,
              :QRB-RATE-34,
              :QRB-AMT-35,
              :QRB-RATE-35,
              :QRB-AMT-36,
              :QRB-RATE-36,
              :QRB-AMT-37,
              :QRB-RATE-37,
              :QRB-AMT-38,
              :QRB-RATE-38,
              :QRB-AMT-39,
              :QRB-RATE-39,
              :QRB-AMT-40,
              :QRB-RATE-40,
              :QRB-AMT-41,
              :QRB-RATE-41,
              :QRB-AMT-42,
              :QRB-RATE-42,
              :QRB-AMT-43,
              :QRB-RATE-43,
              :QRB-AMT-44,
              :QRB-RATE-44,
              :QRB-AMT-45,
              :QRB-RATE-45,
              :QRB-AMT-46,
              :QRB-RATE-46,
              :QRB-AMT-47,
              :QRB-RATE-47,
              :QRB-AMT-48,
              :QRB-RATE-48,
              :QRB-AMT-49,
              :QRB-RATE-49,
              :QRB-AMT-50,
              :QRB-RATE-50,
              :QRB-AMT-51,
              :QRB-RATE-51,
              :QRB-AMT-52,
              :QRB-RATE-52,
              :QRB-AMT-53,
              :QRB-RATE-53,
              :QRB-AMT-54,
              :QRB-RATE-54,
              :QRB-AMT-55,
              :QRB-RATE-55,
              :QRB-AMT-56,
              :QRB-RATE-56,
              :QRB-AMT-57,
              :QRB-RATE-57,
              :QRB-AMT-58,
              :QRB-RATE-58,
              :QRB-AMT-59,
              :QRB-RATE-59,
              :QRB-AMT-60,
              :QRB-RATE-60,
              :QRB-AMT-61,
              :QRB-RATE-61,
              :QRB-AMT-62,
              :QRB-RATE-62,
              :QRB-AMT-63,
              :QRB-RATE-63,
              :QRB-AMT-64,
              :QRB-RATE-64,
              :QRB-AMT-65,
              :QRB-RATE-65,
              :QRB-AMT-66,
              :QRB-RATE-66,
              :QRB-AMT-67,
              :QRB-RATE-67,
              :QRB-AMT-68,
              :QRB-RATE-68,
              :QRB-AMT-69,
              :QRB-RATE-69,
              :QRB-AMT-70,
              :QRB-RATE-70,
              :QRB-AMT-71,
              :QRB-RATE-71,
              :QRB-AMT-72,
              :QRB-RATE-72,
              :QRB-AMT-73,
              :QRB-RATE-73,
              :QRB-AMT-74,
              :QRB-RATE-74,
              :QRB-AMT-75,
              :QRB-RATE-75,
              :QRB-LTOUCH-DATE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-RB1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-RB1-FILE.

           ACCEPT RB-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE RB-PATH-SQL TO E-FILE.
           MOVE RB1-KEY TO E-KEYX.
           PERFORM SET-RB-FIELDS.

           EXEC SQL
            UPDATE DBO.RBFILE
             SET
              RBFILE.RB_TBL_NO = :QRB-TBL-NO,
              RBFILE.RB_TERM = :QRB-TERM,
              RBFILE.RB_MON_DAY_CD = :QRB-MON-DAY-CD,
              RBFILE.RB_AMT_1 = :QRB-AMT-1,
              RBFILE.RB_RATE_1 = :QRB-RATE-1,
              RBFILE.RB_AMT_2 = :QRB-AMT-2,
              RBFILE.RB_RATE_2 = :QRB-RATE-2,
              RBFILE.RB_AMT_3 = :QRB-AMT-3,
              RBFILE.RB_RATE_3 = :QRB-RATE-3,
              RBFILE.RB_AMT_4 = :QRB-AMT-4,
              RBFILE.RB_RATE_4 = :QRB-RATE-4,
              RBFILE.RB_AMT_5 = :QRB-AMT-5,
              RBFILE.RB_RATE_5 = :QRB-RATE-5,
              RBFILE.RB_AMT_6 = :QRB-AMT-6,
              RBFILE.RB_RATE_6 = :QRB-RATE-6,
              RBFILE.RB_AMT_7 = :QRB-AMT-7,
              RBFILE.RB_RATE_7 = :QRB-RATE-7,
              RBFILE.RB_AMT_8 = :QRB-AMT-8,
              RBFILE.RB_RATE_8 = :QRB-RATE-8,
              RBFILE.RB_AMT_9 = :QRB-AMT-9,
              RBFILE.RB_RATE_9 = :QRB-RATE-9,
              RBFILE.RB_AMT_10 = :QRB-AMT-10,
              RBFILE.RB_RATE_10 = :QRB-RATE-10,
              RBFILE.RB_AMT_11 = :QRB-AMT-11,
              RBFILE.RB_RATE_11 = :QRB-RATE-11,
              RBFILE.RB_AMT_12 = :QRB-AMT-12,
              RBFILE.RB_RATE_12 = :QRB-RATE-12,
              RBFILE.RB_AMT_13 = :QRB-AMT-13,
              RBFILE.RB_RATE_13 = :QRB-RATE-13,
              RBFILE.RB_AMT_14 = :QRB-AMT-14,
              RBFILE.RB_RATE_14 = :QRB-RATE-14,
              RBFILE.RB_AMT_15 = :QRB-AMT-15,
              RBFILE.RB_RATE_15 = :QRB-RATE-15,
              RBFILE.RB_AMT_16 = :QRB-AMT-16,
              RBFILE.RB_RATE_16 = :QRB-RATE-16,
              RBFILE.RB_AMT_17 = :QRB-AMT-17,
              RBFILE.RB_RATE_17 = :QRB-RATE-17,
              RBFILE.RB_AMT_18 = :QRB-AMT-18,
              RBFILE.RB_RATE_18 = :QRB-RATE-18,
              RBFILE.RB_AMT_19 = :QRB-AMT-19,
              RBFILE.RB_RATE_19 = :QRB-RATE-19,
              RBFILE.RB_AMT_20 = :QRB-AMT-20,
              RBFILE.RB_RATE_20 = :QRB-RATE-20,
              RBFILE.RB_AMT_21 = :QRB-AMT-21,
              RBFILE.RB_RATE_21 = :QRB-RATE-21,
              RBFILE.RB_AMT_22 = :QRB-AMT-22,
              RBFILE.RB_RATE_22 = :QRB-RATE-22,
              RBFILE.RB_AMT_23 = :QRB-AMT-23,
              RBFILE.RB_RATE_23 = :QRB-RATE-23,
              RBFILE.RB_AMT_24 = :QRB-AMT-24,
              RBFILE.RB_RATE_24 = :QRB-RATE-24,
              RBFILE.RB_AMT_25 = :QRB-AMT-25,
              RBFILE.RB_RATE_25 = :QRB-RATE-25,
              RBFILE.RB_AMT_26 = :QRB-AMT-26,
              RBFILE.RB_RATE_26 = :QRB-RATE-26,
              RBFILE.RB_AMT_27 = :QRB-AMT-27,
              RBFILE.RB_RATE_27 = :QRB-RATE-27,
              RBFILE.RB_AMT_28 = :QRB-AMT-28,
              RBFILE.RB_RATE_28 = :QRB-RATE-28,
              RBFILE.RB_AMT_29 = :QRB-AMT-29,
              RBFILE.RB_RATE_29 = :QRB-RATE-29,
              RBFILE.RB_AMT_30 = :QRB-AMT-30,
              RBFILE.RB_RATE_30 = :QRB-RATE-30,
              RBFILE.RB_AMT_31 = :QRB-AMT-31,
              RBFILE.RB_RATE_31 = :QRB-RATE-31,
              RBFILE.RB_AMT_32 = :QRB-AMT-32,
              RBFILE.RB_RATE_32 = :QRB-RATE-32,
              RBFILE.RB_AMT_33 = :QRB-AMT-33,
              RBFILE.RB_RATE_33 = :QRB-RATE-33,
              RBFILE.RB_AMT_34 = :QRB-AMT-34,
              RBFILE.RB_RATE_34 = :QRB-RATE-34,
              RBFILE.RB_AMT_35 = :QRB-AMT-35,
              RBFILE.RB_RATE_35 = :QRB-RATE-35,
              RBFILE.RB_AMT_36 = :QRB-AMT-36,
              RBFILE.RB_RATE_36 = :QRB-RATE-36,
              RBFILE.RB_AMT_37 = :QRB-AMT-37,
              RBFILE.RB_RATE_37 = :QRB-RATE-37,
              RBFILE.RB_AMT_38 = :QRB-AMT-38,
              RBFILE.RB_RATE_38 = :QRB-RATE-38,
              RBFILE.RB_AMT_39 = :QRB-AMT-39,
              RBFILE.RB_RATE_39 = :QRB-RATE-39,
              RBFILE.RB_AMT_40 = :QRB-AMT-40,
              RBFILE.RB_RATE_40 = :QRB-RATE-40,
              RBFILE.RB_AMT_41 = :QRB-AMT-41,
              RBFILE.RB_RATE_41 = :QRB-RATE-41,
              RBFILE.RB_AMT_42 = :QRB-AMT-42,
              RBFILE.RB_RATE_42 = :QRB-RATE-42,
              RBFILE.RB_AMT_43 = :QRB-AMT-43,
              RBFILE.RB_RATE_43 = :QRB-RATE-43,
              RBFILE.RB_AMT_44 = :QRB-AMT-44,
              RBFILE.RB_RATE_44 = :QRB-RATE-44,
              RBFILE.RB_AMT_45 = :QRB-AMT-45,
              RBFILE.RB_RATE_45 = :QRB-RATE-45,
              RBFILE.RB_AMT_46 = :QRB-AMT-46,
              RBFILE.RB_RATE_46 = :QRB-RATE-46,
              RBFILE.RB_AMT_47 = :QRB-AMT-47,
              RBFILE.RB_RATE_47 = :QRB-RATE-47,
              RBFILE.RB_AMT_48 = :QRB-AMT-48,
              RBFILE.RB_RATE_48 = :QRB-RATE-48,
              RBFILE.RB_AMT_49 = :QRB-AMT-49,
              RBFILE.RB_RATE_49 = :QRB-RATE-49,
              RBFILE.RB_AMT_50 = :QRB-AMT-50,
              RBFILE.RB_RATE_50 = :QRB-RATE-50,
              RBFILE.RB_AMT_51 = :QRB-AMT-51,
              RBFILE.RB_RATE_51 = :QRB-RATE-51,
              RBFILE.RB_AMT_52 = :QRB-AMT-52,
              RBFILE.RB_RATE_52 = :QRB-RATE-52,
              RBFILE.RB_AMT_53 = :QRB-AMT-53,
              RBFILE.RB_RATE_53 = :QRB-RATE-53,
              RBFILE.RB_AMT_54 = :QRB-AMT-54,
              RBFILE.RB_RATE_54 = :QRB-RATE-54,
              RBFILE.RB_AMT_55 = :QRB-AMT-55,
              RBFILE.RB_RATE_55 = :QRB-RATE-55,
              RBFILE.RB_AMT_56 = :QRB-AMT-56,
              RBFILE.RB_RATE_56 = :QRB-RATE-56,
              RBFILE.RB_AMT_57 = :QRB-AMT-57,
              RBFILE.RB_RATE_57 = :QRB-RATE-57,
              RBFILE.RB_AMT_58 = :QRB-AMT-58,
              RBFILE.RB_RATE_58 = :QRB-RATE-58,
              RBFILE.RB_AMT_59 = :QRB-AMT-59,
              RBFILE.RB_RATE_59 = :QRB-RATE-59,
              RBFILE.RB_AMT_60 = :QRB-AMT-60,
              RBFILE.RB_RATE_60 = :QRB-RATE-60,
              RBFILE.RB_AMT_61 = :QRB-AMT-61,
              RBFILE.RB_RATE_61 = :QRB-RATE-61,
              RBFILE.RB_AMT_62 = :QRB-AMT-62,
              RBFILE.RB_RATE_62 = :QRB-RATE-62,
              RBFILE.RB_AMT_63 = :QRB-AMT-63,
              RBFILE.RB_RATE_63 = :QRB-RATE-63,
              RBFILE.RB_AMT_64 = :QRB-AMT-64,
              RBFILE.RB_RATE_64 = :QRB-RATE-64,
              RBFILE.RB_AMT_65 = :QRB-AMT-65,
              RBFILE.RB_RATE_65 = :QRB-RATE-65,
              RBFILE.RB_AMT_66 = :QRB-AMT-66,
              RBFILE.RB_RATE_66 = :QRB-RATE-66,
              RBFILE.RB_AMT_67 = :QRB-AMT-67,
              RBFILE.RB_RATE_67 = :QRB-RATE-67,
              RBFILE.RB_AMT_68 = :QRB-AMT-68,
              RBFILE.RB_RATE_68 = :QRB-RATE-68,
              RBFILE.RB_AMT_69 = :QRB-AMT-69,
              RBFILE.RB_RATE_69 = :QRB-RATE-69,
              RBFILE.RB_AMT_70 = :QRB-AMT-70,
              RBFILE.RB_RATE_70 = :QRB-RATE-70,
              RBFILE.RB_AMT_71 = :QRB-AMT-71,
              RBFILE.RB_RATE_71 = :QRB-RATE-71,
              RBFILE.RB_AMT_72 = :QRB-AMT-72,
              RBFILE.RB_RATE_72 = :QRB-RATE-72,
              RBFILE.RB_AMT_73 = :QRB-AMT-73,
              RBFILE.RB_RATE_73 = :QRB-RATE-73,
              RBFILE.RB_AMT_74 = :QRB-AMT-74,
              RBFILE.RB_RATE_74 = :QRB-RATE-74,
              RBFILE.RB_AMT_75 = :QRB-AMT-75,
              RBFILE.RB_RATE_75 = :QRB-RATE-75,
              RBFILE.RB_LTOUCH_DATE = :QRB-LTOUCH-DATE
            WHERE RBFILE.RB_TBL_NO = :QRB-TBL-NO
              AND RBFILE.RB_TERM = :QRB-TERM
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-RB1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-RB1-FILE.

           PERFORM DELETE-IT.
           MOVE RB-PATH-SQL TO E-FILE.
           MOVE RB1-KEY TO E-KEYX.
           PERFORM SET-RB-FIELDS.

           EXEC SQL
            DELETE FROM DBO.RBFILE
            WHERE RBFILE.RB_TBL_NO = :QRB-TBL-NO
              AND RBFILE.RB_TERM = :QRB-TERM
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-RB1-FILE.

      *-----------------------------------------------------------------
       CLOSE-RB1-FILE.

           PERFORM CLOSE-IT.

           IF ( RB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE RB1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO RB1-CURSOR-STAT.

