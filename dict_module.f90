MODULE DICTIONARY 

  implicit none 

  real,    parameter, public :: r_missing = -999.
  integer, parameter, public :: i_missing = -999

  private ! Make sure everything is hidden, unless we make it 
          ! publically accessible 

  integer, parameter, private :: value_length = 80
  character(len=1), dimension(1) :: char4transfer 

  type KEY_VALUE 
    character(len=40)             :: key 
    character(len=1), allocatable :: value(:)
    character(len=3)              :: type
  end type KEY_VALUE 

  type DICT 
    private 
    type(KEY_VALUE), dimension(:), allocatable :: list 
    type(KEY_VALUE), dimension(:), allocatable :: old
  end type DICT 

! We always want this one to be private 

  type(DICT), private :: dict_data 

! Public functions/subroutines 

  public :: dict_create      ! subroutine
  public :: dict_delete      ! subroutine
  public :: dict_set         ! function
  public :: dict_get         ! function
  public :: dict_print       ! function
  public :: dict_from_file   ! function
  public :: dict_delete_key  ! function

! Interface definitions

 INTERFACE dict_get
    module procedure dict_get_INT, dict_get_FLT, dict_get_STR, dict_get_LOG
 END INTERFACE

 INTERFACE dict_set
    module procedure dict_set_INT, dict_set_FLT, dict_set_STR, dict_set_LOG
 END INTERFACE

 CONTAINS 

!-------------------------------------------------------------------------------
  SUBROUTINE DICT_CREATE( key_value, int, flt, str, log ) 

   character(len=*)           :: key_value
   character(len=*), optional :: str
   real, optional             :: flt
   integer, optional          :: int
   logical, optional          :: log

   integer n, m, length
   logical ret

   IF( .not. allocated(dict_data%list) ) THEN

    n = 0
    allocate(dict_data%list(1))

   ELSE

    n = size(dict_data%list)

! Check to make sure the key_value is not already created

    length = len(key_value)
    DO m = 1,size(dict_data%list)
      IF( dict_data%list(m)%key(1:length) .EQ. key_value(1:length) ) THEN
        write(6,*)
        write(6,*) 'DICT_CREATE:  WARNING, WARNING, WARNING'
        write(6,*) 'DICT_CREATE:  KEY ',trim(key_value),' IS ALREADY EXISTS - RETURNING'
        write(6,*)
        RETURN
      ENDIF
    ENDDO

    IF( allocated(dict_data%old) ) deallocate(dict_data%old) 
    allocate(dict_data%old(n))                                ! Temporary allocate the old list
    dict_data%old(:) = dict_data%list(:)                      ! Copy current into old
    deallocate(dict_data%list)                                ! Deallocate current list
    allocate(dict_data%list(n+1))                             ! Allocate new list
    dict_data%list(1:n) = dict_data%old(1:n)                  ! Copy current into old
    deallocate(dict_data%old)                                 ! Deallocate old list

   ENDIF


   dict_data%list(n+1)%key   = key_value

   IF ( present(int) ) THEN
     ret = dict_set(key_value, int)
     dict_data%list(n+1)%type = 'int'
   ELSEIF( present(flt) ) THEN
     ret = dict_set(key_value, flt)
     dict_data%list(n+1)%type = 'flt'
   ELSEIF( present(str) ) THEN
     ret = dict_set(key_value, str)
     dict_data%list(n+1)%type = 'str'
   ELSEIF( present(log) ) THEN
     ret = dict_set(key_value, log)
     dict_data%list(n+1)%type = 'log'
   ELSE
     ret = dict_set(key_value, "NULL")
     dict_data%list(n+1)%type = 'str'
   ENDIF


  END SUBROUTINE 

!-------------------------------------------------------------------------------
  SUBROUTINE DICT_DELETE( )

   IF( .not. allocated(dict_data%list) ) THEN

    write(6,*)
    write(6,*) 'DICT_DELETE:  WARNING, WARNING, WARNING'
    write(6,*) 'DICT_DELETE:  DICTIONARY DOES NOT EXIST!'
    write(6,*)
    RETURN

   ELSE

    deallocate(dict_data%list)                                ! Deallocate current list

   ENDIF

  END SUBROUTINE 

!-------------------------------------------------------------------------------
  FUNCTION DICT_DELETE_KEY( key_value ) result(ret)

   character(len=*) :: key_value
   logical          :: ret

   integer l, n, m, length

   IF( .not. allocated(dict_data%list) ) THEN

    write(6,*)
    write(6,*) 'DICT_DELETE_KEY:  WARNING, WARNING, WARNING'
    write(6,*) 'DICT_DELETE_KEY:  DICTIONARY DOES NOT EXIST!'
    write(6,*)
    ret = .false.
    RETURN

   ELSE

    n = size(dict_data%list)

    IF( allocated(dict_data%old) ) deallocate(dict_data%old)  ! Make sure temporary dict is deallocated
    allocate(dict_data%old(n))                                ! Temporary allocate the old list
    dict_data%old(:) = dict_data%list(:)                      ! Copy current into old
    deallocate(dict_data%list)                                ! Deallocate main dictionary list
    allocate(dict_data%list(n-1))                             ! Allocate new dictionary list with one less element

! Copy dictionary, omitting key_value entry 

    length = len(key_value)
    l = 1
    DO m = 1,n
      IF( dict_data%old(m)%key(1:length) .NE. key_value(1:length) ) THEN
        dict_data%list(l) = dict_data%old(m)                  ! Copy current into old
        l = l + 1
      ENDIF
    ENDDO

    deallocate(dict_data%old)                                 ! Deallocate old list
    ret = .true.

   ENDIF

  END FUNCTION

!-------------------------------------------------------------------------------
  SUBROUTINE DICT_PRINT( )

   integer                     :: n, int
   real                        :: flt
   character(len=value_length) :: str
   logical                     :: log

   write(6,*) 
   write(6,*) '======================= BEGIN PARAMETER DICTIONARY  ==========================='
   write(6,*) 

   IF( .not. allocated(dict_data%list) ) THEN
    
    write(6,*)
    write(6,*) '                 DICT_PRINT:  WARNING, WARNING, WARNING'
    write(6,*) '                 DICT_PRINT:  DICTIONARY DOES NOT EXIST!'
    write(6,*)

   ELSE

    DO n = 1,size(dict_data%list)

     IF( dict_data%list(n)%type(1:3) .eq. 'str' ) THEN
      str = ""
      str = DICT_GET( dict_data%list(n)%key, str )
      write(6,FMT='(a40," -->",1x,a10)')  trim(dict_data%list(n)%key), trim(str)
     ENDIF

     IF( dict_data%list(n)%type(1:3) .eq. 'flt' ) THEN
      flt = DICT_GET( dict_data%list(n)%key, flt )
      write(6,FMT='(a40," -->",1x,g15.5)')  trim(dict_data%list(n)%key), flt
     ENDIF

     IF( dict_data%list(n)%type(1:3) .eq. 'int' ) THEN
      int = DICT_GET( dict_data%list(n)%key, int )
      write(6,FMT='(a40," -->",1x,i10)')  trim(dict_data%list(n)%key), int
     ENDIF

     IF( dict_data%list(n)%type(1:3) .eq. 'log' ) THEN
      log = DICT_GET( dict_data%list(n)%key, log )
      write(6,FMT='(a40," -->",1x,l10)')  trim(dict_data%list(n)%key), log
     ENDIF

    ENDDO

   ENDIF

   write(6,*) 
   write(6,*) '======================== END PARAMETER DICTIONARY  ============================'
   write(6,*) 

  RETURN
  END SUBROUTINE

!-------------------------------------------------------------------------------
  FUNCTION DICT_SET_INT( key, value ) result(ret)

   character(len=*) key
   integer value
   logical ret

   integer length, lengthData, n

   length = len(key)

   DO n = 1,size(dict_data%list)

    IF( dict_data%list(n)%key(1:length) .EQ. key(1:length) ) THEN
       IF( allocated(dict_data%list(n)%value) ) THEN
         deallocate(dict_data%list(n)%value)
       ENDIF
       lengthData = size(transfer(value, char4transfer))
       allocate(dict_data%list(n)%value(lengthData))
       dict_data%list(n)%value = transfer(value, dict_data%list(n)%value)
       dict_data%list(n)%type  = 'int'
       ret = .true.
       RETURN
     ENDIF

   ENDDO

   write(6,*) 'DICT_SET:  WARNING, WARNING, WARNING'
   write(6,*) 'DICT_SET:  KEY=[',trim(key),'] DOES NOT EXIST - RETURNING'
   write(6,*) 'DICT_SET:  WARNING, WARNING, WARNING'
   ret = .false.

  RETURN
  END FUNCTION

!-------------------------------------------------------------------------------
  FUNCTION DICT_SET_FLT( key, value ) result(ret)

   character(len=*) key
   real value
   logical ret

   integer length, lengthData, n

   length = len(key)

   DO n = 1,size(dict_data%list)

    IF( dict_data%list(n)%key(1:length) .EQ. key(1:length) ) THEN
       IF( allocated(dict_data%list(n)%value) ) THEN
         deallocate(dict_data%list(n)%value)
       ENDIF
       lengthData = size(transfer(value, char4transfer))
       allocate(dict_data%list(n)%value(lengthData))
       dict_data%list(n)%value = transfer(value, dict_data%list(n)%value)
       dict_data%list(n)%type  = 'flt'
       ret = .true.
       RETURN
     ENDIF

   ENDDO

   write(6,*) 'DICT_SET:  WARNING, WARNING, WARNING'
   write(6,*) 'DICT_SET:  KEY=[',trim(key),'] DOES NOT EXIST - RETURNING'
   write(6,*) 'DICT_SET:  WARNING, WARNING, WARNING'
   ret = .false.

  RETURN
  END FUNCTION

!-------------------------------------------------------------------------------
  FUNCTION DICT_SET_STR( key, value ) result(ret)

   character(len=*) key, value
   logical ret

   integer length, lengthData, n

   length = len(key)

   DO n = 1,size(dict_data%list)

    IF( dict_data%list(n)%key(1:length) .EQ. key(1:length) ) THEN
       IF( allocated(dict_data%list(n)%value) ) THEN
         deallocate(dict_data%list(n)%value)
       ENDIF
       lengthData = len(value)
       allocate(dict_data%list(n)%value(lengthData))
       dict_data%list(n)%value = transfer(value(1:lengthData), dict_data%list(n)%value)
       dict_data%list(n)%type  = 'str'
       ret = .true.
       RETURN
     ENDIF

   ENDDO

   write(6,*) 'DICT_SET:  WARNING, WARNING, WARNING'
   write(6,*) 'DICT_SET:  KEY=[',trim(key),'] DOES NOT EXIST - RETURNING'
   write(6,*) 'DICT_SET:  WARNING, WARNING, WARNING'
   ret = .false.

  RETURN
  END FUNCTION

!-------------------------------------------------------------------------------
  FUNCTION DICT_SET_LOG( key, value ) result(ret)

   character(len=*) key
   logical value
   logical ret

   integer length, lengthData, n

   length = len(key)

   DO n = 1,size(dict_data%list)

    IF( dict_data%list(n)%key(1:length) .EQ. key(1:length) ) THEN
       IF( allocated(dict_data%list(n)%value) ) THEN
         deallocate(dict_data%list(n)%value)
       ENDIF
       lengthData = size(transfer(value, char4transfer))
       allocate(dict_data%list(n)%value(lengthData))
       dict_data%list(n)%value = transfer(value, dict_data%list(n)%value)
       dict_data%list(n)%type  = 'log'
       ret = .true.
       RETURN
     ENDIF

   ENDDO

   write(6,*) 'DICT_SET:  WARNING, WARNING, WARNING'
   write(6,*) 'DICT_SET:  KEY=[',trim(key),'] DOES NOT EXIST - RETURNING'
   write(6,*) 'DICT_SET:  WARNING, WARNING, WARNING'
   ret = .false.

  RETURN
  END FUNCTION

!-------------------------------------------------------------------------------
  FUNCTION DICT_GET_STR( key, kind ) result(string)

    character(len=*) :: key, kind
    character(len=value_length) :: string
! DCD 12/9/10  removed unused variable ret
    integer length, n

    length = len(key)

    DO n = 1,size(dict_data%list)

     IF( dict_data%list(n)%key(1:length) .EQ. key(1:length) ) THEN
       length = len(transfer(dict_data%list(n)%value, string))
       string = transfer(dict_data%list(n)%value, string)
       RETURN
     ENDIF

    ENDDO

    string(1:5) = '-999'

  RETURN
  END FUNCTION 

!-------------------------------------------------------------------------------
  FUNCTION DICT_GET_FLT( key, kind ) result(value)

    real value, kind
    character(len=*) :: key 
    
    integer length, n

    length = len(key)

    DO n = 1,size(dict_data%list)

     IF( dict_data%list(n)%key(1:length) .EQ. key(1:length) ) THEN
       value = transfer(dict_data%list(n)%value, value)
       RETURN
     ENDIF

    ENDDO

    value = -999.

  RETURN
  END FUNCTION 

!-------------------------------------------------------------------------------
  FUNCTION DICT_GET_INT( key, kind ) result(value)

    character(len=*) :: key 
    integer value, kind
    integer length, n

    length = len(key)

    DO n = 1,size(dict_data%list)

     IF( dict_data%list(n)%key(1:length) .EQ. key(1:length) ) THEN
       value = transfer(dict_data%list(n)%value, value)
       RETURN
     ENDIF

    ENDDO

    value = -999

  RETURN
  END FUNCTION 

!-------------------------------------------------------------------------------
  FUNCTION DICT_GET_LOG( key, kind ) result(value)

    character(len=*) :: key 
    logical          :: value, kind
    integer length, n

    length = len(key)

    DO n = 1,size(dict_data%list)

     IF( dict_data%list(n)%key(1:length) .EQ. key(1:length) ) THEN
       value = transfer(dict_data%list(n)%value, value)
       RETURN
     ENDIF

    ENDDO

    value = .false.

  RETURN
  END FUNCTION 

!-------------------------------------------------------------------------------
  FUNCTION DICT_FROM_FILE( filename ) result(ret)

    character(len=*)  :: filename
    logical ret

    character(len=46) :: key 
    integer           :: int
    real              :: flt
    character(len=80) :: str
    logical           :: log
    character(len= 3) :: type
    character(len= 1) :: first_char
    logical           :: if_exist

! DCD 12/9/10  removed variable ios
    integer line

    INQUIRE(file=trim(filename), exist=if_exist)

    IF( .not. if_exist ) THEN
     write(6,*) 'DICT_FROM_FILE:  WARNING, WARNING, WARNING'
     write(6,*) 'DICT_FROM_FILE: ',trim(filename),' DOES NOT EXIST - EXITING!'
     write(6,*) 'DICT_FROM_FILE:  WARNING, WARNING, WARNING'
     STOP
    ELSE

     OPEN(1,file=filename, status='old', form='formatted')

    ENDIF

    line = 1
    DO 

     print *, 'DICT_FROM_FILE:  reading line # ', line
     line = line + 1

     read(1,'(a1)',ADVANCE='NO',END=999) first_char
     IF( first_char .eq. "#") THEN
      read(1,*)
      CYCLE
     ENDIF

     print *, 'DICT_FROM_FILE:  read 1st char ', line

     BACKSPACE 1

     read(1,FMT='(a)',ADVANCE='NO',END=999) key
     type = key(43:45)
     key(43:45) = '   '

     print *, 'DICT_FROM_FILE:  read char 1:45 ', line

     IF( type .eq. 'flt' ) THEN
      read(1,*) flt
      CALL DICT_CREATE(trim(key), flt=flt)
     ENDIF
     IF( type .eq. 'int' ) THEN
      read(1,*) int
      CALL DICT_CREATE(trim(key), int=int)
     ENDIF
     IF( type .eq. 'str' ) THEN
      read(1,'(a)') str
      CALL DICT_CREATE(trim(key), str=str)
     ENDIF
     IF( type .eq. 'log' ) THEN
      read(1,*) log
      CALL DICT_CREATE(trim(key), log=log)
     ENDIF
     IF( type .eq. '' .or. type .eq. '   ' ) THEN
      print *, 'DICT_FROM_FILE:  UNKNOWN TYPE, [KEY/TYPE] = ', key,type
     ENDIF

    ENDDO

999 CONTINUE

    ret = .true.

  RETURN
  END FUNCTION 

END MODULE 
