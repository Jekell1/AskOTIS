      * COPYMEMBER: LIBSP/SPCPGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - CPFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD CP-REC FROM QCP-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-CP-FIELDS.
           MOVE QCP-BRNO TO CP-BRNO.
           MOVE QCP-ID TO CP-ID.

           IF ( QCP-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO CP-LTOUCH-DATE
           ELSE
              MOVE QCP-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CP-LTOUCH-DATE.

           MOVE QCP-DESC TO CP-DESC.
           MOVE QCP-COL-ID-1 TO CP-COL-ID(1).
           MOVE QCP-COL-ID-2 TO CP-COL-ID(2).
           MOVE QCP-COL-ID-3 TO CP-COL-ID(3).
           MOVE QCP-COL-ID-4 TO CP-COL-ID(4).
           MOVE QCP-COL-ID-5 TO CP-COL-ID(5).
           MOVE QCP-COL-ID-6 TO CP-COL-ID(6).
           MOVE QCP-COL-ID-7 TO CP-COL-ID(7).
           MOVE QCP-COL-ID-8 TO CP-COL-ID(8).
           MOVE QCP-COL-ID-9 TO CP-COL-ID(9).
           MOVE QCP-COL-ID-10 TO CP-COL-ID(10).
           MOVE QCP-COL-ID-11 TO CP-COL-ID(11).
           MOVE QCP-COL-ID-12 TO CP-COL-ID(12).
           MOVE QCP-COL-ID-13 TO CP-COL-ID(13).
           MOVE QCP-COL-ID-14 TO CP-COL-ID(14).
           MOVE QCP-COL-ID-15 TO CP-COL-ID(15).
           MOVE QCP-COL-ID-16 TO CP-COL-ID(16).
           MOVE QCP-COL-ID-17 TO CP-COL-ID(17).
           MOVE QCP-COL-ID-18 TO CP-COL-ID(18).
           MOVE QCP-COL-ID-19 TO CP-COL-ID(19).
           MOVE QCP-COL-ID-20 TO CP-COL-ID(20).
           MOVE QCP-COL-ID-21 TO CP-COL-ID(21).
           MOVE QCP-COL-ID-22 TO CP-COL-ID(22).
           MOVE QCP-COL-ID-23 TO CP-COL-ID(23).
           MOVE QCP-COL-ID-24 TO CP-COL-ID(24).
           MOVE QCP-COL-ID-25 TO CP-COL-ID(25).
           MOVE QCP-COL-ID-26 TO CP-COL-ID(26).
           MOVE QCP-COL-ID-27 TO CP-COL-ID(27).
           MOVE QCP-COL-ID-28 TO CP-COL-ID(28).
           MOVE QCP-COL-ID-29 TO CP-COL-ID(29).
           MOVE QCP-COL-ID-30 TO CP-COL-ID(30).
           MOVE QCP-COL-ID-31 TO CP-COL-ID(31).
           MOVE QCP-COL-ID-32 TO CP-COL-ID(32).
           MOVE QCP-COL-ID-33 TO CP-COL-ID(33).
           MOVE QCP-COL-ID-34 TO CP-COL-ID(34).
           MOVE QCP-COL-ID-35 TO CP-COL-ID(35).
           MOVE QCP-COL-ID-36 TO CP-COL-ID(36).
           MOVE QCP-COL-ID-37 TO CP-COL-ID(37).
           MOVE QCP-COL-ID-38 TO CP-COL-ID(38).
           MOVE QCP-COL-ID-39 TO CP-COL-ID(39).
           MOVE QCP-COL-ID-40 TO CP-COL-ID(40).
           MOVE QCP-COL-ID-41 TO CP-COL-ID(41).
           MOVE QCP-COL-ID-42 TO CP-COL-ID(42).
           MOVE QCP-COL-ID-43 TO CP-COL-ID(43).
           MOVE QCP-COL-ID-44 TO CP-COL-ID(44).
           MOVE QCP-COL-ID-45 TO CP-COL-ID(45).
           MOVE QCP-COL-ID-46 TO CP-COL-ID(46).
           MOVE QCP-COL-ID-47 TO CP-COL-ID(47).
           MOVE QCP-COL-ID-48 TO CP-COL-ID(48).
           MOVE QCP-COL-ID-49 TO CP-COL-ID(49).
           MOVE QCP-COL-ID-50 TO CP-COL-ID(50).
           MOVE QCP-COL-ID-51 TO CP-COL-ID(51).
           MOVE QCP-COL-ID-52 TO CP-COL-ID(52).
           MOVE QCP-COL-ID-53 TO CP-COL-ID(53).
           MOVE QCP-COL-ID-54 TO CP-COL-ID(54).
           MOVE QCP-COL-ID-55 TO CP-COL-ID(55).
           MOVE QCP-COL-ID-56 TO CP-COL-ID(56).
           MOVE QCP-COL-ID-57 TO CP-COL-ID(57).
           MOVE QCP-COL-ID-58 TO CP-COL-ID(58).
           MOVE QCP-COL-ID-59 TO CP-COL-ID(59).
           MOVE QCP-COL-ID-60 TO CP-COL-ID(60).
           MOVE QCP-COL-ID-61 TO CP-COL-ID(61).
           MOVE QCP-COL-ID-62 TO CP-COL-ID(62).
           MOVE QCP-COL-ID-63 TO CP-COL-ID(63).
           MOVE QCP-COL-ID-64 TO CP-COL-ID(64).
           MOVE QCP-COL-ID-65 TO CP-COL-ID(65).
           MOVE QCP-COL-ID-66 TO CP-COL-ID(66).
           MOVE QCP-COL-ID-67 TO CP-COL-ID(67).
           MOVE QCP-COL-ID-68 TO CP-COL-ID(68).
           MOVE QCP-COL-ID-69 TO CP-COL-ID(69).
           MOVE QCP-COL-ID-70 TO CP-COL-ID(70).
           MOVE QCP-COL-ID-71 TO CP-COL-ID(71).
           MOVE QCP-COL-ID-72 TO CP-COL-ID(72).
           MOVE QCP-COL-ID-73 TO CP-COL-ID(73).
           MOVE QCP-COL-ID-74 TO CP-COL-ID(74).
           MOVE QCP-COL-ID-75 TO CP-COL-ID(75).
           MOVE QCP-COL-ID-76 TO CP-COL-ID(76).
           MOVE QCP-COL-ID-77 TO CP-COL-ID(77).
           MOVE QCP-COL-ID-78 TO CP-COL-ID(78).
           MOVE QCP-COL-ID-79 TO CP-COL-ID(79).
           MOVE QCP-COL-ID-80 TO CP-COL-ID(80).
           MOVE QCP-ALPHA-FLAG TO CP-ALPHA-FLAG.
           MOVE QCP-TOTAL-ACCOUNTS TO CP-TOTAL-ACCOUNTS.
           MOVE QCP-COLLECTOR-COUNT TO CP-COLLECTOR-COUNT.
           MOVE QCP-LAST-SUBSCRIPT TO CP-LAST-SUBSCRIPT.
 
      *-----------------------------------------------------------------
      *    LOAD QCP-REC FROM CP-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-CP-FIELDS.
           MOVE CP-BRNO TO QCP-BRNO.
           MOVE CP-ID TO QCP-ID.

           IF ( CP-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCP-LTOUCH-DATE
           ELSE
              MOVE CP-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCP-LTOUCH-DATE.

           MOVE CP-DESC TO QCP-DESC.
           MOVE CP-COL-ID(1) TO QCP-COL-ID-1.
           MOVE CP-COL-ID(2) TO QCP-COL-ID-2.
           MOVE CP-COL-ID(3) TO QCP-COL-ID-3.
           MOVE CP-COL-ID(4) TO QCP-COL-ID-4.
           MOVE CP-COL-ID(5) TO QCP-COL-ID-5.
           MOVE CP-COL-ID(6) TO QCP-COL-ID-6.
           MOVE CP-COL-ID(7) TO QCP-COL-ID-7.
           MOVE CP-COL-ID(8) TO QCP-COL-ID-8.
           MOVE CP-COL-ID(9) TO QCP-COL-ID-9.
           MOVE CP-COL-ID(10) TO QCP-COL-ID-10.
           MOVE CP-COL-ID(11) TO QCP-COL-ID-11.
           MOVE CP-COL-ID(12) TO QCP-COL-ID-12.
           MOVE CP-COL-ID(13) TO QCP-COL-ID-13.
           MOVE CP-COL-ID(14) TO QCP-COL-ID-14.
           MOVE CP-COL-ID(15) TO QCP-COL-ID-15.
           MOVE CP-COL-ID(16) TO QCP-COL-ID-16.
           MOVE CP-COL-ID(17) TO QCP-COL-ID-17.
           MOVE CP-COL-ID(18) TO QCP-COL-ID-18.
           MOVE CP-COL-ID(19) TO QCP-COL-ID-19.
           MOVE CP-COL-ID(20) TO QCP-COL-ID-20.
           MOVE CP-COL-ID(21) TO QCP-COL-ID-21.
           MOVE CP-COL-ID(22) TO QCP-COL-ID-22.
           MOVE CP-COL-ID(23) TO QCP-COL-ID-23.
           MOVE CP-COL-ID(24) TO QCP-COL-ID-24.
           MOVE CP-COL-ID(25) TO QCP-COL-ID-25.
           MOVE CP-COL-ID(26) TO QCP-COL-ID-26.
           MOVE CP-COL-ID(27) TO QCP-COL-ID-27.
           MOVE CP-COL-ID(28) TO QCP-COL-ID-28.
           MOVE CP-COL-ID(29) TO QCP-COL-ID-29.
           MOVE CP-COL-ID(30) TO QCP-COL-ID-30.
           MOVE CP-COL-ID(31) TO QCP-COL-ID-31.
           MOVE CP-COL-ID(32) TO QCP-COL-ID-32.
           MOVE CP-COL-ID(33) TO QCP-COL-ID-33.
           MOVE CP-COL-ID(34) TO QCP-COL-ID-34.
           MOVE CP-COL-ID(35) TO QCP-COL-ID-35.
           MOVE CP-COL-ID(36) TO QCP-COL-ID-36.
           MOVE CP-COL-ID(37) TO QCP-COL-ID-37.
           MOVE CP-COL-ID(38) TO QCP-COL-ID-38.
           MOVE CP-COL-ID(39) TO QCP-COL-ID-39.
           MOVE CP-COL-ID(40) TO QCP-COL-ID-40.
           MOVE CP-COL-ID(41) TO QCP-COL-ID-41.
           MOVE CP-COL-ID(42) TO QCP-COL-ID-42.
           MOVE CP-COL-ID(43) TO QCP-COL-ID-43.
           MOVE CP-COL-ID(44) TO QCP-COL-ID-44.
           MOVE CP-COL-ID(45) TO QCP-COL-ID-45.
           MOVE CP-COL-ID(46) TO QCP-COL-ID-46.
           MOVE CP-COL-ID(47) TO QCP-COL-ID-47.
           MOVE CP-COL-ID(48) TO QCP-COL-ID-48.
           MOVE CP-COL-ID(49) TO QCP-COL-ID-49.
           MOVE CP-COL-ID(50) TO QCP-COL-ID-50.
           MOVE CP-COL-ID(51) TO QCP-COL-ID-51.
           MOVE CP-COL-ID(52) TO QCP-COL-ID-52.
           MOVE CP-COL-ID(53) TO QCP-COL-ID-53.
           MOVE CP-COL-ID(54) TO QCP-COL-ID-54.
           MOVE CP-COL-ID(55) TO QCP-COL-ID-55.
           MOVE CP-COL-ID(56) TO QCP-COL-ID-56.
           MOVE CP-COL-ID(57) TO QCP-COL-ID-57.
           MOVE CP-COL-ID(58) TO QCP-COL-ID-58.
           MOVE CP-COL-ID(59) TO QCP-COL-ID-59.
           MOVE CP-COL-ID(60) TO QCP-COL-ID-60.
           MOVE CP-COL-ID(61) TO QCP-COL-ID-61.
           MOVE CP-COL-ID(62) TO QCP-COL-ID-62.
           MOVE CP-COL-ID(63) TO QCP-COL-ID-63.
           MOVE CP-COL-ID(64) TO QCP-COL-ID-64.
           MOVE CP-COL-ID(65) TO QCP-COL-ID-65.
           MOVE CP-COL-ID(66) TO QCP-COL-ID-66.
           MOVE CP-COL-ID(67) TO QCP-COL-ID-67.
           MOVE CP-COL-ID(68) TO QCP-COL-ID-68.
           MOVE CP-COL-ID(69) TO QCP-COL-ID-69.
           MOVE CP-COL-ID(70) TO QCP-COL-ID-70.
           MOVE CP-COL-ID(71) TO QCP-COL-ID-71.
           MOVE CP-COL-ID(72) TO QCP-COL-ID-72.
           MOVE CP-COL-ID(73) TO QCP-COL-ID-73.
           MOVE CP-COL-ID(74) TO QCP-COL-ID-74.
           MOVE CP-COL-ID(75) TO QCP-COL-ID-75.
           MOVE CP-COL-ID(76) TO QCP-COL-ID-76.
           MOVE CP-COL-ID(77) TO QCP-COL-ID-77.
           MOVE CP-COL-ID(78) TO QCP-COL-ID-78.
           MOVE CP-COL-ID(79) TO QCP-COL-ID-79.
           MOVE CP-COL-ID(80) TO QCP-COL-ID-80.
           MOVE CP-ALPHA-FLAG TO QCP-ALPHA-FLAG.
           MOVE CP-TOTAL-ACCOUNTS TO QCP-TOTAL-ACCOUNTS.
           MOVE CP-COLLECTOR-COUNT TO QCP-COLLECTOR-COUNT.
           MOVE CP-LAST-SUBSCRIPT TO QCP-LAST-SUBSCRIPT.
 
