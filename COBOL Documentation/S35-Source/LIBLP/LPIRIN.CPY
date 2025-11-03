      * COPYMEMBER: LIBLP/LPIRIN
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
       WRITE-IR-FILE.

           ACCEPT IR-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE IR-PATH-SQL TO E-FILE.
           MOVE IR-KEY TO E-KEYX.
           PERFORM SET-IR-FIELDS.

           EXEC SQL
            INSERT INTO DBO.IRFILE
             (IRFILE.IR_KEY,
              IRFILE.IR_LTOUCH_DATE,
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
              IRFILE.IR_DESC)
            VALUES
             (:QIR-KEY,
              :QIR-LTOUCH-DATE,
              :QIR-PER-FG,
              :QIR-INSRATE-1,
              :QIR-INSRATE-2,
              :QIR-INSRATE-3,
              :QIR-INSRATE-4,
              :QIR-INSRATE-5,
              :QIR-INSRATE-6,
              :QIR-INSRATE-7,
              :QIR-INSRATE-8,
              :QIR-INSRATE-9,
              :QIR-INSRATE-10,
              :QIR-INSRATE-11,
              :QIR-INSRATE-12,
              :QIR-INSRATE-13,
              :QIR-INSRATE-14,
              :QIR-INSRATE-15,
              :QIR-INSRATE-16,
              :QIR-INSRATE-17,
              :QIR-INSRATE-18,
              :QIR-INSRATE-19,
              :QIR-INSRATE-20,
              :QIR-INSRATE-21,
              :QIR-INSRATE-22,
              :QIR-INSRATE-23,
              :QIR-INSRATE-24,
              :QIR-INSRATE-25,
              :QIR-INSRATE-26,
              :QIR-INSRATE-27,
              :QIR-INSRATE-28,
              :QIR-INSRATE-29,
              :QIR-INSRATE-30,
              :QIR-INSRATE-31,
              :QIR-INSRATE-32,
              :QIR-INSRATE-33,
              :QIR-INSRATE-34,
              :QIR-INSRATE-35,
              :QIR-INSRATE-36,
              :QIR-INSRATE-37,
              :QIR-INSRATE-38,
              :QIR-INSRATE-39,
              :QIR-INSRATE-40,
              :QIR-INSRATE-41,
              :QIR-INSRATE-42,
              :QIR-INSRATE-43,
              :QIR-INSRATE-44,
              :QIR-INSRATE-45,
              :QIR-INSRATE-46,
              :QIR-INSRATE-47,
              :QIR-INSRATE-48,
              :QIR-INSRATE-49,
              :QIR-INSRATE-50,
              :QIR-INSRATE-51,
              :QIR-INSRATE-52,
              :QIR-INSRATE-53,
              :QIR-INSRATE-54,
              :QIR-INSRATE-55,
              :QIR-INSRATE-56,
              :QIR-INSRATE-57,
              :QIR-INSRATE-58,
              :QIR-INSRATE-59,
              :QIR-INSRATE-60,
              :QIR-INSRATE-61,
              :QIR-INSRATE-62,
              :QIR-INSRATE-63,
              :QIR-INSRATE-64,
              :QIR-INSRATE-65,
              :QIR-INSRATE-66,
              :QIR-INSRATE-67,
              :QIR-INSRATE-68,
              :QIR-INSRATE-69,
              :QIR-INSRATE-70,
              :QIR-INSRATE-71,
              :QIR-INSRATE-72,
              :QIR-INSRATE-73,
              :QIR-INSRATE-74,
              :QIR-INSRATE-75,
              :QIR-INSRATE-76,
              :QIR-INSRATE-77,
              :QIR-INSRATE-78,
              :QIR-INSRATE-79,
              :QIR-INSRATE-80,
              :QIR-INSRATE-81,
              :QIR-INSRATE-82,
              :QIR-INSRATE-83,
              :QIR-INSRATE-84,
              :QIR-INSRATE-85,
              :QIR-INSRATE-86,
              :QIR-INSRATE-87,
              :QIR-INSRATE-88,
              :QIR-INSRATE-89,
              :QIR-INSRATE-90,
              :QIR-INSRATE-91,
              :QIR-INSRATE-92,
              :QIR-INSRATE-93,
              :QIR-INSRATE-94,
              :QIR-INSRATE-95,
              :QIR-INSRATE-96,
              :QIR-INSRATE-97,
              :QIR-INSRATE-98,
              :QIR-INSRATE-99,
              :QIR-INSRATE-100,
              :QIR-INSRATE-101,
              :QIR-INSRATE-102,
              :QIR-INSRATE-103,
              :QIR-INSRATE-104,
              :QIR-INSRATE-105,
              :QIR-INSRATE-106,
              :QIR-INSRATE-107,
              :QIR-INSRATE-108,
              :QIR-INSRATE-109,
              :QIR-INSRATE-110,
              :QIR-INSRATE-111,
              :QIR-INSRATE-112,
              :QIR-INSRATE-113,
              :QIR-INSRATE-114,
              :QIR-INSRATE-115,
              :QIR-INSRATE-116,
              :QIR-INSRATE-117,
              :QIR-INSRATE-118,
              :QIR-INSRATE-119,
              :QIR-INSRATE-120,
              :QIR-DESC)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-IR-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-IR-FILE.

           ACCEPT IR-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE IR-PATH-SQL TO E-FILE.
           MOVE IR-KEY TO E-KEYX.
           PERFORM SET-IR-FIELDS.

           EXEC SQL
            UPDATE DBO.IRFILE
             SET
              IRFILE.IR_KEY = :QIR-KEY,
              IRFILE.IR_LTOUCH_DATE = :QIR-LTOUCH-DATE,
              IRFILE.IR_PER_FG = :QIR-PER-FG,
              IRFILE.IR_INSRATE_1 = :QIR-INSRATE-1,
              IRFILE.IR_INSRATE_2 = :QIR-INSRATE-2,
              IRFILE.IR_INSRATE_3 = :QIR-INSRATE-3,
              IRFILE.IR_INSRATE_4 = :QIR-INSRATE-4,
              IRFILE.IR_INSRATE_5 = :QIR-INSRATE-5,
              IRFILE.IR_INSRATE_6 = :QIR-INSRATE-6,
              IRFILE.IR_INSRATE_7 = :QIR-INSRATE-7,
              IRFILE.IR_INSRATE_8 = :QIR-INSRATE-8,
              IRFILE.IR_INSRATE_9 = :QIR-INSRATE-9,
              IRFILE.IR_INSRATE_10 = :QIR-INSRATE-10,
              IRFILE.IR_INSRATE_11 = :QIR-INSRATE-11,
              IRFILE.IR_INSRATE_12 = :QIR-INSRATE-12,
              IRFILE.IR_INSRATE_13 = :QIR-INSRATE-13,
              IRFILE.IR_INSRATE_14 = :QIR-INSRATE-14,
              IRFILE.IR_INSRATE_15 = :QIR-INSRATE-15,
              IRFILE.IR_INSRATE_16 = :QIR-INSRATE-16,
              IRFILE.IR_INSRATE_17 = :QIR-INSRATE-17,
              IRFILE.IR_INSRATE_18 = :QIR-INSRATE-18,
              IRFILE.IR_INSRATE_19 = :QIR-INSRATE-19,
              IRFILE.IR_INSRATE_20 = :QIR-INSRATE-20,
              IRFILE.IR_INSRATE_21 = :QIR-INSRATE-21,
              IRFILE.IR_INSRATE_22 = :QIR-INSRATE-22,
              IRFILE.IR_INSRATE_23 = :QIR-INSRATE-23,
              IRFILE.IR_INSRATE_24 = :QIR-INSRATE-24,
              IRFILE.IR_INSRATE_25 = :QIR-INSRATE-25,
              IRFILE.IR_INSRATE_26 = :QIR-INSRATE-26,
              IRFILE.IR_INSRATE_27 = :QIR-INSRATE-27,
              IRFILE.IR_INSRATE_28 = :QIR-INSRATE-28,
              IRFILE.IR_INSRATE_29 = :QIR-INSRATE-29,
              IRFILE.IR_INSRATE_30 = :QIR-INSRATE-30,
              IRFILE.IR_INSRATE_31 = :QIR-INSRATE-31,
              IRFILE.IR_INSRATE_32 = :QIR-INSRATE-32,
              IRFILE.IR_INSRATE_33 = :QIR-INSRATE-33,
              IRFILE.IR_INSRATE_34 = :QIR-INSRATE-34,
              IRFILE.IR_INSRATE_35 = :QIR-INSRATE-35,
              IRFILE.IR_INSRATE_36 = :QIR-INSRATE-36,
              IRFILE.IR_INSRATE_37 = :QIR-INSRATE-37,
              IRFILE.IR_INSRATE_38 = :QIR-INSRATE-38,
              IRFILE.IR_INSRATE_39 = :QIR-INSRATE-39,
              IRFILE.IR_INSRATE_40 = :QIR-INSRATE-40,
              IRFILE.IR_INSRATE_41 = :QIR-INSRATE-41,
              IRFILE.IR_INSRATE_42 = :QIR-INSRATE-42,
              IRFILE.IR_INSRATE_43 = :QIR-INSRATE-43,
              IRFILE.IR_INSRATE_44 = :QIR-INSRATE-44,
              IRFILE.IR_INSRATE_45 = :QIR-INSRATE-45,
              IRFILE.IR_INSRATE_46 = :QIR-INSRATE-46,
              IRFILE.IR_INSRATE_47 = :QIR-INSRATE-47,
              IRFILE.IR_INSRATE_48 = :QIR-INSRATE-48,
              IRFILE.IR_INSRATE_49 = :QIR-INSRATE-49,
              IRFILE.IR_INSRATE_50 = :QIR-INSRATE-50,
              IRFILE.IR_INSRATE_51 = :QIR-INSRATE-51,
              IRFILE.IR_INSRATE_52 = :QIR-INSRATE-52,
              IRFILE.IR_INSRATE_53 = :QIR-INSRATE-53,
              IRFILE.IR_INSRATE_54 = :QIR-INSRATE-54,
              IRFILE.IR_INSRATE_55 = :QIR-INSRATE-55,
              IRFILE.IR_INSRATE_56 = :QIR-INSRATE-56,
              IRFILE.IR_INSRATE_57 = :QIR-INSRATE-57,
              IRFILE.IR_INSRATE_58 = :QIR-INSRATE-58,
              IRFILE.IR_INSRATE_59 = :QIR-INSRATE-59,
              IRFILE.IR_INSRATE_60 = :QIR-INSRATE-60,
              IRFILE.IR_INSRATE_61 = :QIR-INSRATE-61,
              IRFILE.IR_INSRATE_62 = :QIR-INSRATE-62,
              IRFILE.IR_INSRATE_63 = :QIR-INSRATE-63,
              IRFILE.IR_INSRATE_64 = :QIR-INSRATE-64,
              IRFILE.IR_INSRATE_65 = :QIR-INSRATE-65,
              IRFILE.IR_INSRATE_66 = :QIR-INSRATE-66,
              IRFILE.IR_INSRATE_67 = :QIR-INSRATE-67,
              IRFILE.IR_INSRATE_68 = :QIR-INSRATE-68,
              IRFILE.IR_INSRATE_69 = :QIR-INSRATE-69,
              IRFILE.IR_INSRATE_70 = :QIR-INSRATE-70,
              IRFILE.IR_INSRATE_71 = :QIR-INSRATE-71,
              IRFILE.IR_INSRATE_72 = :QIR-INSRATE-72,
              IRFILE.IR_INSRATE_73 = :QIR-INSRATE-73,
              IRFILE.IR_INSRATE_74 = :QIR-INSRATE-74,
              IRFILE.IR_INSRATE_75 = :QIR-INSRATE-75,
              IRFILE.IR_INSRATE_76 = :QIR-INSRATE-76,
              IRFILE.IR_INSRATE_77 = :QIR-INSRATE-77,
              IRFILE.IR_INSRATE_78 = :QIR-INSRATE-78,
              IRFILE.IR_INSRATE_79 = :QIR-INSRATE-79,
              IRFILE.IR_INSRATE_80 = :QIR-INSRATE-80,
              IRFILE.IR_INSRATE_81 = :QIR-INSRATE-81,
              IRFILE.IR_INSRATE_82 = :QIR-INSRATE-82,
              IRFILE.IR_INSRATE_83 = :QIR-INSRATE-83,
              IRFILE.IR_INSRATE_84 = :QIR-INSRATE-84,
              IRFILE.IR_INSRATE_85 = :QIR-INSRATE-85,
              IRFILE.IR_INSRATE_86 = :QIR-INSRATE-86,
              IRFILE.IR_INSRATE_87 = :QIR-INSRATE-87,
              IRFILE.IR_INSRATE_88 = :QIR-INSRATE-88,
              IRFILE.IR_INSRATE_89 = :QIR-INSRATE-89,
              IRFILE.IR_INSRATE_90 = :QIR-INSRATE-90,
              IRFILE.IR_INSRATE_91 = :QIR-INSRATE-91,
              IRFILE.IR_INSRATE_92 = :QIR-INSRATE-92,
              IRFILE.IR_INSRATE_93 = :QIR-INSRATE-93,
              IRFILE.IR_INSRATE_94 = :QIR-INSRATE-94,
              IRFILE.IR_INSRATE_95 = :QIR-INSRATE-95,
              IRFILE.IR_INSRATE_96 = :QIR-INSRATE-96,
              IRFILE.IR_INSRATE_97 = :QIR-INSRATE-97,
              IRFILE.IR_INSRATE_98 = :QIR-INSRATE-98,
              IRFILE.IR_INSRATE_99 = :QIR-INSRATE-99,
              IRFILE.IR_INSRATE_100 = :QIR-INSRATE-100,
              IRFILE.IR_INSRATE_101 = :QIR-INSRATE-101,
              IRFILE.IR_INSRATE_102 = :QIR-INSRATE-102,
              IRFILE.IR_INSRATE_103 = :QIR-INSRATE-103,
              IRFILE.IR_INSRATE_104 = :QIR-INSRATE-104,
              IRFILE.IR_INSRATE_105 = :QIR-INSRATE-105,
              IRFILE.IR_INSRATE_106 = :QIR-INSRATE-106,
              IRFILE.IR_INSRATE_107 = :QIR-INSRATE-107,
              IRFILE.IR_INSRATE_108 = :QIR-INSRATE-108,
              IRFILE.IR_INSRATE_109 = :QIR-INSRATE-109,
              IRFILE.IR_INSRATE_110 = :QIR-INSRATE-110,
              IRFILE.IR_INSRATE_111 = :QIR-INSRATE-111,
              IRFILE.IR_INSRATE_112 = :QIR-INSRATE-112,
              IRFILE.IR_INSRATE_113 = :QIR-INSRATE-113,
              IRFILE.IR_INSRATE_114 = :QIR-INSRATE-114,
              IRFILE.IR_INSRATE_115 = :QIR-INSRATE-115,
              IRFILE.IR_INSRATE_116 = :QIR-INSRATE-116,
              IRFILE.IR_INSRATE_117 = :QIR-INSRATE-117,
              IRFILE.IR_INSRATE_118 = :QIR-INSRATE-118,
              IRFILE.IR_INSRATE_119 = :QIR-INSRATE-119,
              IRFILE.IR_INSRATE_120 = :QIR-INSRATE-120,
              IRFILE.IR_DESC = :QIR-DESC
             WHERE IRFILE.IR_KEY = :QIR-KEY
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-IR-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-IR-FILE.

           PERFORM DELETE-IT.
           MOVE IR-PATH-SQL TO E-FILE.
           MOVE IR-KEY TO E-KEYX.
           PERFORM SET-IR-FIELDS.

           EXEC SQL
            DELETE FROM DBO.IRFILE
             WHERE IRFILE.IR_KEY = :QIR-KEY
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-IR-FILE.

      *-----------------------------------------------------------------
       CLOSE-IR-FILE.

           PERFORM CLOSE-IT.

           IF ( IR-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE IR_CURSOR
              END-EXEC
              MOVE STAT---BAD TO IR-CURSOR-STAT.

