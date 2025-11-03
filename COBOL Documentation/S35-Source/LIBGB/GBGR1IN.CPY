      * COPYMEMBER: LIBGB/GBGR1IN
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
       WRITE-GR1-FILE.

           ACCEPT GR-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE GR-PATH-SQL TO E-FILE.
           MOVE GR1-KEY TO E-KEYX.
           PERFORM SET-GR-FIELDS.

           EXEC SQL
            INSERT INTO DBO.GRFILE
             (GRFILE.GR_ID,
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
              GRFILE.GR_LTOUCH_DATE)
            VALUES
             (:QGR-ID,
              :QGR-NAME,
              :QGR-LEVEL,
              :QGR-MEMBER-ID-1,
              :QGR-MEMBER-ID-2,
              :QGR-MEMBER-ID-3,
              :QGR-MEMBER-ID-4,
              :QGR-MEMBER-ID-5,
              :QGR-MEMBER-ID-6,
              :QGR-MEMBER-ID-7,
              :QGR-MEMBER-ID-8,
              :QGR-MEMBER-ID-9,
              :QGR-MEMBER-ID-10,
              :QGR-MEMBER-ID-11,
              :QGR-MEMBER-ID-12,
              :QGR-MEMBER-ID-13,
              :QGR-MEMBER-ID-14,
              :QGR-MEMBER-ID-15,
              :QGR-MEMBER-ID-16,
              :QGR-MEMBER-ID-17,
              :QGR-MEMBER-ID-18,
              :QGR-MEMBER-ID-19,
              :QGR-MEMBER-ID-20,
              :QGR-MEMBER-ID-21,
              :QGR-MEMBER-ID-22,
              :QGR-MEMBER-ID-23,
              :QGR-MEMBER-ID-24,
              :QGR-MEMBER-ID-25,
              :QGR-MEMBER-ID-26,
              :QGR-MEMBER-ID-27,
              :QGR-MEMBER-ID-28,
              :QGR-MEMBER-ID-29,
              :QGR-MEMBER-ID-30,
              :QGR-MEMBER-ID-31,
              :QGR-MEMBER-ID-32,
              :QGR-MEMBER-ID-33,
              :QGR-MEMBER-ID-34,
              :QGR-MEMBER-ID-35,
              :QGR-MEMBER-ID-36,
              :QGR-MEMBER-ID-37,
              :QGR-MEMBER-ID-38,
              :QGR-MEMBER-ID-39,
              :QGR-MEMBER-ID-40,
              :QGR-MEMBER-ID-41,
              :QGR-MEMBER-ID-42,
              :QGR-MEMBER-ID-43,
              :QGR-MEMBER-ID-44,
              :QGR-MEMBER-ID-45,
              :QGR-MEMBER-ID-46,
              :QGR-MEMBER-ID-47,
              :QGR-MEMBER-ID-48,
              :QGR-MEMBER-ID-49,
              :QGR-MEMBER-ID-50,
              :QGR-MEMBER-ID-51,
              :QGR-MEMBER-ID-52,
              :QGR-MEMBER-ID-53,
              :QGR-MEMBER-ID-54,
              :QGR-MEMBER-ID-55,
              :QGR-MEMBER-ID-56,
              :QGR-MEMBER-ID-57,
              :QGR-MEMBER-ID-58,
              :QGR-MEMBER-ID-59,
              :QGR-MEMBER-ID-60,
              :QGR-MEMBER-ID-61,
              :QGR-MEMBER-ID-62,
              :QGR-MEMBER-ID-63,
              :QGR-MEMBER-ID-64,
              :QGR-MEMBER-ID-65,
              :QGR-MEMBER-ID-66,
              :QGR-MEMBER-ID-67,
              :QGR-MEMBER-ID-68,
              :QGR-MEMBER-ID-69,
              :QGR-MEMBER-ID-70,
              :QGR-MEMBER-ID-71,
              :QGR-MEMBER-ID-72,
              :QGR-MEMBER-ID-73,
              :QGR-MEMBER-ID-74,
              :QGR-MEMBER-ID-75,
              :QGR-MEMBER-ID-76,
              :QGR-MEMBER-ID-77,
              :QGR-MEMBER-ID-78,
              :QGR-MEMBER-ID-79,
              :QGR-MEMBER-ID-80,
              :QGR-MEMBER-ID-81,
              :QGR-MEMBER-ID-82,
              :QGR-MEMBER-ID-83,
              :QGR-MEMBER-ID-84,
              :QGR-MEMBER-ID-85,
              :QGR-MEMBER-ID-86,
              :QGR-MEMBER-ID-87,
              :QGR-MEMBER-ID-88,
              :QGR-MEMBER-ID-89,
              :QGR-MEMBER-ID-90,
              :QGR-MEMBER-ID-91,
              :QGR-MEMBER-ID-92,
              :QGR-MEMBER-ID-93,
              :QGR-MEMBER-ID-94,
              :QGR-MEMBER-ID-95,
              :QGR-MEMBER-ID-96,
              :QGR-MEMBER-ID-97,
              :QGR-MEMBER-ID-98,
              :QGR-MEMBER-ID-99,
              :QGR-MEMBER-ID-100,
              :QGR-CONTACT,
              :QGR-ADDR-1,
              :QGR-ADDR-2,
              :QGR-CITY,
              :QGR-STATE,
              :QGR-ZIP,
              :QGR-PHONE,
              :QGR-HOME-BRANCH,
              :QGR-LN-APPROVAL-LIMIT,
              :QGR-LTOUCH-DATE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-GR1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-GR1-FILE.

           ACCEPT GR-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE GR-PATH-SQL TO E-FILE.
           MOVE GR1-KEY TO E-KEYX.
           PERFORM SET-GR-FIELDS.

           EXEC SQL
            UPDATE DBO.GRFILE
             SET
              GRFILE.GR_ID = :QGR-ID,
              GRFILE.GR_NAME = :QGR-NAME,
              GRFILE.GR_LEVEL = :QGR-LEVEL,
              GRFILE.GR_MEMBER_ID_1 = :QGR-MEMBER-ID-1,
              GRFILE.GR_MEMBER_ID_2 = :QGR-MEMBER-ID-2,
              GRFILE.GR_MEMBER_ID_3 = :QGR-MEMBER-ID-3,
              GRFILE.GR_MEMBER_ID_4 = :QGR-MEMBER-ID-4,
              GRFILE.GR_MEMBER_ID_5 = :QGR-MEMBER-ID-5,
              GRFILE.GR_MEMBER_ID_6 = :QGR-MEMBER-ID-6,
              GRFILE.GR_MEMBER_ID_7 = :QGR-MEMBER-ID-7,
              GRFILE.GR_MEMBER_ID_8 = :QGR-MEMBER-ID-8,
              GRFILE.GR_MEMBER_ID_9 = :QGR-MEMBER-ID-9,
              GRFILE.GR_MEMBER_ID_10 = :QGR-MEMBER-ID-10,
              GRFILE.GR_MEMBER_ID_11 = :QGR-MEMBER-ID-11,
              GRFILE.GR_MEMBER_ID_12 = :QGR-MEMBER-ID-12,
              GRFILE.GR_MEMBER_ID_13 = :QGR-MEMBER-ID-13,
              GRFILE.GR_MEMBER_ID_14 = :QGR-MEMBER-ID-14,
              GRFILE.GR_MEMBER_ID_15 = :QGR-MEMBER-ID-15,
              GRFILE.GR_MEMBER_ID_16 = :QGR-MEMBER-ID-16,
              GRFILE.GR_MEMBER_ID_17 = :QGR-MEMBER-ID-17,
              GRFILE.GR_MEMBER_ID_18 = :QGR-MEMBER-ID-18,
              GRFILE.GR_MEMBER_ID_19 = :QGR-MEMBER-ID-19,
              GRFILE.GR_MEMBER_ID_20 = :QGR-MEMBER-ID-20,
              GRFILE.GR_MEMBER_ID_21 = :QGR-MEMBER-ID-21,
              GRFILE.GR_MEMBER_ID_22 = :QGR-MEMBER-ID-22,
              GRFILE.GR_MEMBER_ID_23 = :QGR-MEMBER-ID-23,
              GRFILE.GR_MEMBER_ID_24 = :QGR-MEMBER-ID-24,
              GRFILE.GR_MEMBER_ID_25 = :QGR-MEMBER-ID-25,
              GRFILE.GR_MEMBER_ID_26 = :QGR-MEMBER-ID-26,
              GRFILE.GR_MEMBER_ID_27 = :QGR-MEMBER-ID-27,
              GRFILE.GR_MEMBER_ID_28 = :QGR-MEMBER-ID-28,
              GRFILE.GR_MEMBER_ID_29 = :QGR-MEMBER-ID-29,
              GRFILE.GR_MEMBER_ID_30 = :QGR-MEMBER-ID-30,
              GRFILE.GR_MEMBER_ID_31 = :QGR-MEMBER-ID-31,
              GRFILE.GR_MEMBER_ID_32 = :QGR-MEMBER-ID-32,
              GRFILE.GR_MEMBER_ID_33 = :QGR-MEMBER-ID-33,
              GRFILE.GR_MEMBER_ID_34 = :QGR-MEMBER-ID-34,
              GRFILE.GR_MEMBER_ID_35 = :QGR-MEMBER-ID-35,
              GRFILE.GR_MEMBER_ID_36 = :QGR-MEMBER-ID-36,
              GRFILE.GR_MEMBER_ID_37 = :QGR-MEMBER-ID-37,
              GRFILE.GR_MEMBER_ID_38 = :QGR-MEMBER-ID-38,
              GRFILE.GR_MEMBER_ID_39 = :QGR-MEMBER-ID-39,
              GRFILE.GR_MEMBER_ID_40 = :QGR-MEMBER-ID-40,
              GRFILE.GR_MEMBER_ID_41 = :QGR-MEMBER-ID-41,
              GRFILE.GR_MEMBER_ID_42 = :QGR-MEMBER-ID-42,
              GRFILE.GR_MEMBER_ID_43 = :QGR-MEMBER-ID-43,
              GRFILE.GR_MEMBER_ID_44 = :QGR-MEMBER-ID-44,
              GRFILE.GR_MEMBER_ID_45 = :QGR-MEMBER-ID-45,
              GRFILE.GR_MEMBER_ID_46 = :QGR-MEMBER-ID-46,
              GRFILE.GR_MEMBER_ID_47 = :QGR-MEMBER-ID-47,
              GRFILE.GR_MEMBER_ID_48 = :QGR-MEMBER-ID-48,
              GRFILE.GR_MEMBER_ID_49 = :QGR-MEMBER-ID-49,
              GRFILE.GR_MEMBER_ID_50 = :QGR-MEMBER-ID-50,
              GRFILE.GR_MEMBER_ID_51 = :QGR-MEMBER-ID-51,
              GRFILE.GR_MEMBER_ID_52 = :QGR-MEMBER-ID-52,
              GRFILE.GR_MEMBER_ID_53 = :QGR-MEMBER-ID-53,
              GRFILE.GR_MEMBER_ID_54 = :QGR-MEMBER-ID-54,
              GRFILE.GR_MEMBER_ID_55 = :QGR-MEMBER-ID-55,
              GRFILE.GR_MEMBER_ID_56 = :QGR-MEMBER-ID-56,
              GRFILE.GR_MEMBER_ID_57 = :QGR-MEMBER-ID-57,
              GRFILE.GR_MEMBER_ID_58 = :QGR-MEMBER-ID-58,
              GRFILE.GR_MEMBER_ID_59 = :QGR-MEMBER-ID-59,
              GRFILE.GR_MEMBER_ID_60 = :QGR-MEMBER-ID-60,
              GRFILE.GR_MEMBER_ID_61 = :QGR-MEMBER-ID-61,
              GRFILE.GR_MEMBER_ID_62 = :QGR-MEMBER-ID-62,
              GRFILE.GR_MEMBER_ID_63 = :QGR-MEMBER-ID-63,
              GRFILE.GR_MEMBER_ID_64 = :QGR-MEMBER-ID-64,
              GRFILE.GR_MEMBER_ID_65 = :QGR-MEMBER-ID-65,
              GRFILE.GR_MEMBER_ID_66 = :QGR-MEMBER-ID-66,
              GRFILE.GR_MEMBER_ID_67 = :QGR-MEMBER-ID-67,
              GRFILE.GR_MEMBER_ID_68 = :QGR-MEMBER-ID-68,
              GRFILE.GR_MEMBER_ID_69 = :QGR-MEMBER-ID-69,
              GRFILE.GR_MEMBER_ID_70 = :QGR-MEMBER-ID-70,
              GRFILE.GR_MEMBER_ID_71 = :QGR-MEMBER-ID-71,
              GRFILE.GR_MEMBER_ID_72 = :QGR-MEMBER-ID-72,
              GRFILE.GR_MEMBER_ID_73 = :QGR-MEMBER-ID-73,
              GRFILE.GR_MEMBER_ID_74 = :QGR-MEMBER-ID-74,
              GRFILE.GR_MEMBER_ID_75 = :QGR-MEMBER-ID-75,
              GRFILE.GR_MEMBER_ID_76 = :QGR-MEMBER-ID-76,
              GRFILE.GR_MEMBER_ID_77 = :QGR-MEMBER-ID-77,
              GRFILE.GR_MEMBER_ID_78 = :QGR-MEMBER-ID-78,
              GRFILE.GR_MEMBER_ID_79 = :QGR-MEMBER-ID-79,
              GRFILE.GR_MEMBER_ID_80 = :QGR-MEMBER-ID-80,
              GRFILE.GR_MEMBER_ID_81 = :QGR-MEMBER-ID-81,
              GRFILE.GR_MEMBER_ID_82 = :QGR-MEMBER-ID-82,
              GRFILE.GR_MEMBER_ID_83 = :QGR-MEMBER-ID-83,
              GRFILE.GR_MEMBER_ID_84 = :QGR-MEMBER-ID-84,
              GRFILE.GR_MEMBER_ID_85 = :QGR-MEMBER-ID-85,
              GRFILE.GR_MEMBER_ID_86 = :QGR-MEMBER-ID-86,
              GRFILE.GR_MEMBER_ID_87 = :QGR-MEMBER-ID-87,
              GRFILE.GR_MEMBER_ID_88 = :QGR-MEMBER-ID-88,
              GRFILE.GR_MEMBER_ID_89 = :QGR-MEMBER-ID-89,
              GRFILE.GR_MEMBER_ID_90 = :QGR-MEMBER-ID-90,
              GRFILE.GR_MEMBER_ID_91 = :QGR-MEMBER-ID-91,
              GRFILE.GR_MEMBER_ID_92 = :QGR-MEMBER-ID-92,
              GRFILE.GR_MEMBER_ID_93 = :QGR-MEMBER-ID-93,
              GRFILE.GR_MEMBER_ID_94 = :QGR-MEMBER-ID-94,
              GRFILE.GR_MEMBER_ID_95 = :QGR-MEMBER-ID-95,
              GRFILE.GR_MEMBER_ID_96 = :QGR-MEMBER-ID-96,
              GRFILE.GR_MEMBER_ID_97 = :QGR-MEMBER-ID-97,
              GRFILE.GR_MEMBER_ID_98 = :QGR-MEMBER-ID-98,
              GRFILE.GR_MEMBER_ID_99 = :QGR-MEMBER-ID-99,
              GRFILE.GR_MEMBER_ID_100 = :QGR-MEMBER-ID-100,
              GRFILE.GR_CONTACT = :QGR-CONTACT,
              GRFILE.GR_ADDR_1 = :QGR-ADDR-1,
              GRFILE.GR_ADDR_2 = :QGR-ADDR-2,
              GRFILE.GR_CITY = :QGR-CITY,
              GRFILE.GR_STATE = :QGR-STATE,
              GRFILE.GR_ZIP = :QGR-ZIP,
              GRFILE.GR_PHONE = :QGR-PHONE,
              GRFILE.GR_HOME_BRANCH = :QGR-HOME-BRANCH,
              GRFILE.GR_LN_APPROVAL_LIMIT = :QGR-LN-APPROVAL-LIMIT,
              GRFILE.GR_LTOUCH_DATE = :QGR-LTOUCH-DATE
             WHERE GRFILE.GR_ID = :QGR-ID
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-GR1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-GR1-FILE.

           PERFORM DELETE-IT.
           MOVE GR-PATH-SQL TO E-FILE.
           MOVE GR1-KEY TO E-KEYX.
           PERFORM SET-GR-FIELDS.

           EXEC SQL
            DELETE FROM DBO.GRFILE
             WHERE GRFILE.GR_ID = :QGR-ID
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-GR1-FILE.

      *-----------------------------------------------------------------
       CLOSE-GR1-FILE.

           PERFORM CLOSE-IT.

           IF ( GR1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GR1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GR1-CURSOR-STAT.
 
