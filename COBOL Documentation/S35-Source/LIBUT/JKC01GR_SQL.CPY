      * COPYMEMBER: LIBGB/GB01GR_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - GROUP BRANCH FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0430 ***NEW***
      *
      ******************************************************************
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  QGR1-WHERE-BEG-KEY.
           05  QGR1-WBEG-ID               PIC X(4).

       01  QGR1-WHERE-END-KEY.
           05  QGR1-WEND-ID               PIC X(4).

       01  QGR-REC.
           03  QGR-ID                    PIC X(4).
           03  QGR-NAME                  PIC X(20).
           03  QGR-LEVEL                 PIC 9.
           03  QGR-MEMBER-ID-1           PIC X(4).
           03  QGR-MEMBER-ID-2           PIC X(4).
           03  QGR-MEMBER-ID-3           PIC X(4).
           03  QGR-MEMBER-ID-4           PIC X(4).
           03  QGR-MEMBER-ID-5           PIC X(4).
           03  QGR-MEMBER-ID-6           PIC X(4).
           03  QGR-MEMBER-ID-7           PIC X(4).
           03  QGR-MEMBER-ID-8           PIC X(4).
           03  QGR-MEMBER-ID-9           PIC X(4).
           03  QGR-MEMBER-ID-10          PIC X(4).
           03  QGR-MEMBER-ID-11           PIC X(4).
           03  QGR-MEMBER-ID-12           PIC X(4).
           03  QGR-MEMBER-ID-13           PIC X(4).
           03  QGR-MEMBER-ID-14           PIC X(4).
           03  QGR-MEMBER-ID-15           PIC X(4).
           03  QGR-MEMBER-ID-16           PIC X(4).
           03  QGR-MEMBER-ID-17           PIC X(4).
           03  QGR-MEMBER-ID-18           PIC X(4).
           03  QGR-MEMBER-ID-19           PIC X(4).
           03  QGR-MEMBER-ID-20           PIC X(4).
           03  QGR-MEMBER-ID-21           PIC X(4).
           03  QGR-MEMBER-ID-22           PIC X(4).
           03  QGR-MEMBER-ID-23           PIC X(4).
           03  QGR-MEMBER-ID-24           PIC X(4).
           03  QGR-MEMBER-ID-25           PIC X(4).
           03  QGR-MEMBER-ID-26           PIC X(4).
           03  QGR-MEMBER-ID-27           PIC X(4).
           03  QGR-MEMBER-ID-28           PIC X(4).
           03  QGR-MEMBER-ID-29           PIC X(4).
           03  QGR-MEMBER-ID-30          PIC X(4).
           03  QGR-MEMBER-ID-31           PIC X(4).
           03  QGR-MEMBER-ID-32           PIC X(4).
           03  QGR-MEMBER-ID-33           PIC X(4).
           03  QGR-MEMBER-ID-34           PIC X(4).
           03  QGR-MEMBER-ID-35           PIC X(4).
           03  QGR-MEMBER-ID-36           PIC X(4).
           03  QGR-MEMBER-ID-37           PIC X(4).
           03  QGR-MEMBER-ID-38           PIC X(4).
           03  QGR-MEMBER-ID-39           PIC X(4).
           03  QGR-MEMBER-ID-40          PIC X(4).
           03  QGR-MEMBER-ID-41           PIC X(4).
           03  QGR-MEMBER-ID-42           PIC X(4).
           03  QGR-MEMBER-ID-43           PIC X(4).
           03  QGR-MEMBER-ID-44           PIC X(4).
           03  QGR-MEMBER-ID-45           PIC X(4).
           03  QGR-MEMBER-ID-46           PIC X(4).
           03  QGR-MEMBER-ID-47           PIC X(4).
           03  QGR-MEMBER-ID-48           PIC X(4).
           03  QGR-MEMBER-ID-49           PIC X(4).
           03  QGR-MEMBER-ID-50          PIC X(4).
           03  QGR-MEMBER-ID-51           PIC X(4).
           03  QGR-MEMBER-ID-52           PIC X(4).
           03  QGR-MEMBER-ID-53           PIC X(4).
           03  QGR-MEMBER-ID-54           PIC X(4).
           03  QGR-MEMBER-ID-55           PIC X(4).
           03  QGR-MEMBER-ID-56           PIC X(4).
           03  QGR-MEMBER-ID-57           PIC X(4).
           03  QGR-MEMBER-ID-58           PIC X(4).
           03  QGR-MEMBER-ID-59           PIC X(4).
           03  QGR-MEMBER-ID-60          PIC X(4).
           03  QGR-MEMBER-ID-61           PIC X(4).
           03  QGR-MEMBER-ID-62           PIC X(4).
           03  QGR-MEMBER-ID-63           PIC X(4).
           03  QGR-MEMBER-ID-64           PIC X(4).
           03  QGR-MEMBER-ID-65           PIC X(4).
           03  QGR-MEMBER-ID-66           PIC X(4).
           03  QGR-MEMBER-ID-67           PIC X(4).
           03  QGR-MEMBER-ID-68           PIC X(4).
           03  QGR-MEMBER-ID-69           PIC X(4).
           03  QGR-MEMBER-ID-70          PIC X(4).
           03  QGR-MEMBER-ID-71           PIC X(4).
           03  QGR-MEMBER-ID-72           PIC X(4).
           03  QGR-MEMBER-ID-73           PIC X(4).
           03  QGR-MEMBER-ID-74           PIC X(4).
           03  QGR-MEMBER-ID-75           PIC X(4).
           03  QGR-MEMBER-ID-76           PIC X(4).
           03  QGR-MEMBER-ID-77           PIC X(4).
           03  QGR-MEMBER-ID-78           PIC X(4).
           03  QGR-MEMBER-ID-79           PIC X(4).
           03  QGR-MEMBER-ID-80          PIC X(4).
           03  QGR-MEMBER-ID-81           PIC X(4).
           03  QGR-MEMBER-ID-82           PIC X(4).
           03  QGR-MEMBER-ID-83           PIC X(4).
           03  QGR-MEMBER-ID-84           PIC X(4).
           03  QGR-MEMBER-ID-85           PIC X(4).
           03  QGR-MEMBER-ID-86           PIC X(4).
           03  QGR-MEMBER-ID-87           PIC X(4).
           03  QGR-MEMBER-ID-88           PIC X(4).
           03  QGR-MEMBER-ID-89           PIC X(4).
           03  QGR-MEMBER-ID-90          PIC X(4).
           03  QGR-MEMBER-ID-91           PIC X(4).
           03  QGR-MEMBER-ID-92           PIC X(4).
           03  QGR-MEMBER-ID-93           PIC X(4).
           03  QGR-MEMBER-ID-94           PIC X(4).
           03  QGR-MEMBER-ID-95           PIC X(4).
           03  QGR-MEMBER-ID-96           PIC X(4).
           03  QGR-MEMBER-ID-97           PIC X(4).
           03  QGR-MEMBER-ID-98           PIC X(4).
           03  QGR-MEMBER-ID-99           PIC X(4).
           03  QGR-MEMBER-ID-100          PIC X(4).
           03  QGR-CONTACT               PIC X(20).
           03  QGR-ADDR-1                PIC X(20).
           03  QGR-ADDR-2                PIC X(20).
           03  QGR-CITY                  PIC X(15).
           03  QGR-STATE                 PIC X(4).
           03  QGR-ZIP                   PIC X(10).
           03  QGR-PHONE                 PIC 9(10).
           03  QGR-HOME-BRANCH           PIC 9(4).
           03  QGR-LN-APPROVAL-LIMIT     PIC 9(7).
           03  QGR-LTOUCH-DATE           PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
