$Regfile="m328def.dat"
$Crystal=20000000
$hwstack=128
$swstack=256
$framesize=512



'___________ Global constants _____________________________________________________________________


' Configuration parameters
Const ReflexStrikeMinTime = 1000
Const ReflexStrikeRndTime = 3000

Const TrainerStrikeCountMin = 50
Const TrainerStrikeCountMax = 500
Const TrainerStrikeCountInc = 10

Const BeepTimeTimer = 300
Const BeepTimeStop = 500
Const BeepTimeStrike = 50
Const BeepTimeBtn = 50
Const BeepTimeBatteryLow = 1000
Const BeepTimeDefaultOff = 100
Const BeepTimeTimeoutOn = 50
Const BeepTimeTimeoutOff = 50

Const BeepOn = 1
Const BeepOff = 0

Const DebounceLimit = 10
Const TimerPeriodInMs = 1
Const TimerReloadValue = 45535

Const LcdNoticeTimeDefault = 1500
Const LcdBackLightTimerStartValue = 30000

' Standard values for I/O handling
Const PinHigh = 1
Const PinLow = 0
Const BtnDown = 1
Const BtnUp = 0


' State machine constants
Const StateMainMenu = 0
Const StateStrikeForce = 1
Const StateStrikeSpeed = 2
Const StateReflex = 3
Const StateTrainer = 4
Const StateDefault = StateStrikeForce
Const StateCount = 4

Const SubStateBackToMenu = 0
Const SubStateDisplayResult = 1

Const SubStateStrikeForceConfigSingle = 2
Const SubStateStrikeForceConfigTen = 3
Const SubStateStrikeForceRunSingle = 4
Const SubStateStrikeForceRunTen = 5

Const SubStateStrikeSpeedConfigShort = 2
Const SubStateStrikeSpeedConfigLong = 3
Const SubStateStrikeSpeedRunShort = 4
Const SubStateStrikeSpeedRunLong = 5

Const SubStateReflexConfig = 2
Const SubStateReflexRun = 3

Const SubStateTrainerConfig = 2
Const SubStateTrainerRun = 3

' Strike speed time constants
Const StrikeSpeedTimeShort = 30000
Const StrikeSpeedTimeLong = 60000

' Battery parameters
Const BatteryCheckInterval = 60000
Const BatteryLevelLow = 720

' Texts
Const ForceUnitText = "N"
Const TimeUnitText = "ms"

Const ForceConstantDivider = 200


'___________ Global variables _____________________________________________________________________

Dim ActualState As Integer
Dim SelectedState As Integer
Dim ActualSubState As Integer

Dim StrikeSpeedMode As Integer

Dim FlagModeButton As Byte                  ' Shows if the MODE button was pressed
Dim FlagSetButton As Byte                   ' Shows if the SET button was pressed
Dim FlagStrike As Byte                      ' Shows if a strike measurement happend
Dim FlagTimer As Byte                       ' Shows if the software timer has expired
Dim FlagBattery As Byte
Dim FlagReflexTimerStarted As Byte          ' Shows if the timer was started when in reflex mode

Dim BtnModeState As Byte                    ' Stable state of the MODE button (needed for debouncing)
Dim BtnModeCntr As Byte                     ' Counter value for MODE button (needed for debouncing)
Dim BtnSetState As Byte                     ' Stable state of the SET button (needed for debouncing)
Dim BtnSetCntr As Byte                      ' Counter value for SET button (needed for debouncing)

Dim AccX As Long
Dim AccY As Long
Dim AccZ As Long
Dim MaxAcc As Long
Dim CurrAcc As Long
Dim MeasurementCount As Integer
Dim AccMeasurements(10) As Long             ' Array for holding 10 measurements for average calculation
Dim MeasurementId As Byte                   ' Current array id
Dim StrikeCount As Byte                     ' Number of strikes


Dim ReflexTime As Dword                     ' Helper variable for storing the current time in reflex mode

Dim TrainerStrikeCount As Integer           ' Stores the strike count set by the user in Trainer mode
Dim TrainerCountSetup As Byte               ' Helper variable which shows if we are in count setup mode

Dim CounterTimer As Word                    ' Helper variable for the software timer
Dim CounterBeep As Byte                     ' Helper variable for the beep sound
Dim BeepIntervalOn As Byte                  ' Shows how many timer cycles the beep sound should last
Dim BeepIntervalOff As Byte                 ' Shows how many timer cycles should be between two beep sounds
Dim BeepCount As Byte                        ' Shows how many beep sounds sould be
Dim BeepState As Byte                        ' Helper variable for the beep sound

Dim CurrentTime As Dword                    ' Shows the time elapsed since startup in ms

Dim ActionResult As Integer

Dim StrikeEnabled As Byte

Dim BatteryTimer As Word
Dim BatteryAdcValue As Word

Dim LcdBackLightTimer as Word

Dim BagWeight As Long
Dim BagWeightTemp As Long
Dim BagWeightInv As Long
Dim EramBagWeight as ERAM Long At &H0
Dim EramBagWeightInv as ERAM Long At &H4

'___________ Subroutine and function declarations _________________________________________________
Declare Sub ChangeState(ByVal newState as Integer)

Declare Sub LcdUpdate()

Declare Sub LcdSetupModeUpdate()

Declare Sub ActionInMainMenu()
Declare Sub ActionBeforeMainMenu()

Declare Sub ActionInStrikeForce()
Declare Sub ActionBeforeStrikeForce()

Declare Sub ActionInStrikeSpeed()
Declare Sub ActionBeforeStrikeSpeed()

Declare Sub ActionInReflex()
Declare Sub ActionBeforeReflex()

Declare Sub ActionInTrainer()
Declare Sub ActionBeforeTrainer()

Declare Sub EnableStriking()
Declare Sub DisableStriking()

Declare Sub EnableLcdBackLight()

Declare Sub BeepMs(ByVal intervalOn As Word)

Declare Sub BeepMsExtended(ByVal intervalOn As Word, ByVal intervalOff As Word, ByVal count As Word)

Declare Sub TimerMs(ByVal timeMs As Word)

Declare Sub LcdNotice(ByVal msg As String, ByVal timeMs As Word)


Declare Function GetResultantForce(ByVal x As Integer, ByVal y As Integer, ByVal z As Integer) As Integer

Declare Function GetFlagModeButton() As Byte

Declare Function GetFlagSetButton() As Byte

Declare Function GetFlagStrike() As Byte

Declare Function GetFlagTimer() As Byte

Declare Function GetFlagBattery() As Byte

'___________ Interrupt config _____________________________________________________________________
Enable Interrupts

Config Int0 = Rising
On Int0 AccelerometerISR
Enable Int0

Config Timer1 = Timer, Prescale = 1,
On Timer1 TimerISR
TIMER1 = TimerReloadValue
Enable Timer1


'___________ Lcd config ___________________________________________________________________________
Config Lcdpin = Pin , Db4 = Portb.0 , Db5 = Portb.1 , Db6 = Portd.6 , Db7 = Portd.7 , E = Portc.1 , Rs = Portc.0
Config Lcd = 16 * 2
Cursor Off Noblink
Cls

'___________ Adc config ___________________________________________________________________________
Config Adc = Single , Prescaler = Auto , Reference = Internal
Start Adc


'___________ Port config __________________________________________________________________________

Config Pind.2 = Input  'int0 - adc_start

Config Pinb.2 = Input
PinModeBtn Alias Pinb.2

Config Pind.3 = Input
PinSetBtn Alias Pind.3


Config Pinc.2 = Output
PinLed Alias Portc.2

Config Pinc.3 = Output
PinBeep Alias Portc.3

Config Pinb.3 = Output
PinXyzReset Alias Portb.3

Config Pinb.5 = Output
PinLcdPower Alias Portb.5

Config Pind.5 = Output
PinPunch Alias Portd.5

Config Pind.4 = Output
PinLcdBackLight Alias Portd.4


'___________ Initial values _______________________________________________________________________
ActualState = StateMainMenu
SelectedState = StateDefault

ActualSubState = 0

FlagModeButton = 0
FlagSetButton = 0
FlagStrike = 0
FlagTimer = 0
FlagReflexTimerStarted = 0

BtnModeState = BtnUp
BtnModeCntr = 0
BtnSetState = BtnUp
BtnSetCntr = 0

AccX = 0
AccY = 0
AccZ = 0
MaxAcc = 0
CurrAcc = 0
MeasurementCount = 0

MeasurementId = 1
StrikeCount = 0

ReflexTime = 0

TrainerStrikeCount = TrainerStrikeCountMin
TrainerCountSetup = 0

CounterTimer = 0
CounterBeep = 0
BeepCount = 0
BeepIntervalOff = 0
BeepIntervalOn = 0
BeepState = 0

CurrentTime = 0

ActionResult  = 0

StrikeEnabled = 0

LcdBackLightTimer = 0

' Read bag weight from EEPROM
BagWeight = EramBagWeight
BagWeightInv = EramBagWeightInv

'___________ Startup ______________________________________________________________________________


Set PinLed
Set PinBeep
Set PinLcdBackLight
Reset PinLcdPower

Set PinXyzReset
Set PinPunch
Waitms 1
Reset PinXyzReset
Reset PinPunch

' Show LCD welcome screen
Cls
Home
Lcd "-Strike Trainer-"
Waitms 2000

' Go to setup mode if both buttons are pressed or the stored bag weight is not valid

BagWeightTemp = Not BagWeightInv
If GetFlagModeButton() = 1 And GetFlagSetButton() = 1 Or BagWeight <> BagWeightTemp Then
    If BagWeight <> BagWeightTemp Then
        BagWeight = 5
    Endif
    
    Call LcdSetupModeUpdate()
    
    Do
        If GetFlagModeButton() = 1 Then
            BagWeight = BagWeight MOD 250
            BagWeight = BagWeight + 5
            Call LcdSetupModeUpdate()
        Endif
        
        If GetFlagSetButton() = 1 Then
            ' Store new bag weight variable into EEPROM
            BagWeightInv = Not BagWeight
            EramBagWeight = BagWeight
            EramBagWeightInv = BagWeightInv
            Exit Do
        Endif
    Loop
Endif

Call LcdUpdate()        ' Show the initial display


'___________ Main Program Loop ____________________________________________________________________
Do
    ' Call the action handler of the actual state
    Select Case ActualState
        Case StateMainMenu
            Call ActionInMainMenu()

        Case StateStrikeForce
            Call ActionInStrikeForce()

        Case StateStrikeSpeed
            Call ActionInStrikeSpeed()

        Case StateReflex
            Call ActionInReflex()

        Case StateTrainer
            Call ActionInTrainer()
    End Select

    If GetFlagBattery() = 1 Then
        BatteryAdcValue = Getadc(7)
        If BatteryAdcValue < BatteryLevelLow Then
            Call BeepMs(BeepTimeBatteryLow)
            Call LcdNotice("  Low Battery   ", LcdNoticeTimeDefault)
        Endif
    Endif
Loop
End


'___________ Subroutine definitions _______________________________________________________________

Sub ChangeState(ByVal newState as Integer)

    ' Change the global state variable
    ActualState = newState

    ' Call the before action handler of the current state
    Select Case ActualState
        Case StateMainMenu
            Call ActionBeforeMainMenu()

        Case StateStrikeForce
            Call ActionBeforeStrikeForce()

        Case StateStrikeSpeed
            Call ActionBeforeStrikeSpeed()

        Case StateReflex
            Call ActionBeforeReflex()

        Case StateTrainer
            Call ActionBeforeTrainer()
    End Select

    Call LcdUpdate()
End Sub


Sub LcdUpdate()
    Cls
    Upperline

    Select Case ActualState
        Case StateMainMenu
            Lcd "[Main Menu]"
            Lowerline
            
            Select Case SelectedState
                Case StateStrikeForce
                    Lcd "Strike Force"

                Case StateStrikeSpeed
                    Lcd "Strike Speed"

                Case StateReflex
                    Lcd "Reflex"

                Case StateTrainer
                    Lcd "Trainer"
            End Select

        Case StateStrikeForce
            Lcd "[Strike Force]"
            Lowerline
            
            Select Case ActualSubState
                Case SubStateStrikeForceConfigSingle
                    Lcd "Single Strike"

                Case SubStateStrikeForceConfigTen
                    Lcd "Ten Strikes"

                Case SubStateStrikeForceRunSingle
                    Lcd "Strike!"

                Case SubStateStrikeForceRunTen
                    Lcd "[" ; StrikeCount ; "/10] "
                    If StrikeCount > 0 Then
                        Lcd AccMeasurements(StrikeCount) ; ForceUnitText
                    Endif

                Case SubStateBackToMenu
                    Lcd "Back"

                Case SubStateDisplayResult
                    If StrikeCount = 1 Then
                        Lcd "[1/1]: " ; ActionResult ; ForceUnitText                        
                    Elseif MeasurementId > 10 Then
                        Lcd "[Avg]: " ; ActionResult ; ForceUnitText
                    Else
                        Lcd "[" ; MeasurementId ; "/10]: " ; AccMeasurements(MeasurementId) ; ForceUnitText
                    Endif
            End Select

        Case StateStrikeSpeed
            Lcd "[Strike Speed]"
            Lowerline
            
            Select Case ActualSubState
                Case SubStateStrikeSpeedConfigShort
                    Lcd "Half Minute"

                Case SubStateStrikeSpeedConfigLong
                    Lcd "One Minute"

                Case SubStateStrikeSpeedRunShort
                    Lcd "Count: " ; StrikeCount

                Case SubStateStrikeSpeedRunLong
                    Lcd "Count: " ; StrikeCount

                Case SubStateBackToMenu
                    Lcd "Back"

                Case SubStateDisplayResult
                    Lcd "Result: " ; ActionResult
            End Select

        Case StateReflex
            Lcd "[Reflex]"
            Lowerline
            
            Select Case ActualSubState
                Case SubStateReflexConfig
                    Lcd "Start"

                Case SubStateReflexRun
                    Lcd "[" ; StrikeCount ; "/10] "
                    If StrikeCount > 0 Then
                        Lcd AccMeasurements(StrikeCount) ; TimeUnitText
                    Endif

                Case SubStateBackToMenu
                    Lcd "Back"

                Case SubStateDisplayResult
                    If MeasurementId > 10 Then
                        Lcd "[Avg]: " ; ActionResult ; TimeUnitText
                    Else
                        Lcd "[" ; MeasurementId ; "/10]: " ; AccMeasurements(MeasurementId) ; TimeUnitText
                    Endif
            End Select

        Case StateTrainer
            Lcd "[Trainer]"
            Lowerline
            
            Select Case ActualSubState
                Case SubStateTrainerConfig
                    If TrainerCountSetup = 0 Then
                        Lcd "Set Count"
                    Else
                        Lcd TrainerStrikeCount ; " [" ; TrainerStrikeCountMin ; "-" ; TrainerStrikeCountMax ; "]"
                    Endif

                Case SubStateTrainerRun
                    Lcd "Count: " ; StrikeCount ; "/" ; TrainerStrikeCount

                Case SubStateBackToMenu
                    Lcd "Back"

                Case SubStateDisplayResult
                    Lcd ActionResult ; " strikes done"
            End Select

    End Select
End Sub

Sub LcdSetupModeUpdate()
    Cls
    Upperline
    Lcd "--Weight Setup--"
    
    Lowerline
    Lcd BagWeight ; " kg"
End Sub

'___________ State action subroutines _____________________________________________________________
Sub ActionInMainMenu()
    If GetFlagModeButton() = 1 Then                    ' If the MODE button is pressed, change the selected substate
        If SelectedState = StateCount Then
            SelectedState = 1
        Else
            Incr SelectedState
        Endif
        Call LcdUpdate()

    Elseif GetFlagSetButton() = 1 Then                 ' If the SET button is pressed, go to the selected substate
        Call ChangeState(SelectedState)
    Endif
End Sub



Sub ActionInStrikeForce()
    Local Index As Integer
    Local TempStr As String * 8

    ' Change substate if one of the buttons were pressed
    If GetFlagModeButton() = 1 Then
        Select Case ActualSubState
            Case SubStateStrikeForceConfigSingle
                ActualSubState = SubStateStrikeForceConfigTen

            Case SubStateStrikeForceConfigTen
                ActualSubState = SubStateBackToMenu

            Case SubStateStrikeForceRunSingle
                Call DisableStriking()
                ActualSubState = SubStateStrikeForceConfigSingle

            Case SubStateStrikeForceRunTen
                Call DisableStriking()
                ActualSubState = SubStateStrikeForceConfigTen

            Case SubStateBackToMenu
                ActualSubState = SubStateStrikeForceConfigSingle

            Case SubStateDisplayResult
                Call ChangeState(StateMainMenu)
                Return
        End Select

        Call LcdUpdate()
    Endif

    If GetFlagSetButton() = 1 Then
        Select Case ActualSubState
            Case SubStateStrikeForceConfigSingle                
                MeasurementId = 1
                StrikeCount = 0
                Call EnableStriking()
                ActualSubState = SubStateStrikeForceRunSingle

            Case SubStateStrikeForceConfigTen
                MeasurementId = 1
                StrikeCount = 0
                Call EnableStriking()
                ActualSubState = SubStateStrikeForceRunTen

            Case SubStateBackToMenu
                Call ChangeState(StateMainMenu)
                Return

            Case SubStateDisplayResult
                If MeasurementId = 11 Then
                    MeasurementId = 1
                Elseif StrikeCount > 1 Then
                    Incr MeasurementId
                Endif
        End Select

        Call LcdUpdate()
    Endif

    ' Check if there is an unprocessed strike
    If GetFlagStrike() = 1 Then
        Incr StrikeCount
        
        If ActualSubState = SubStateStrikeForceRunSingle Then
            ActionResult = MaxAcc * BagWeight
            ActionResult = ActionResult / ForceConstantDivider
            Call DisableStriking()
            Call BeepMsExtended(BeepTimeStop, BeepTimeDefaultOff, 2)
            ActualSubState = SubStateDisplayResult

        Elseif ActualSubState = SubStateStrikeForceRunTen Then
            AccMeasurements(MeasurementId) = MaxAcc * BagWeight
            AccMeasurements(MeasurementId) = AccMeasurements(MeasurementId) / ForceConstantDivider
            Incr MeasurementId

            If MeasurementId > 10 Then
                Call DisableStriking()
                Call BeepMsExtended(BeepTimeStop, BeepTimeDefaultOff, 2)

                ActionResult = 0
                For Index = 1 to 10
                    ActionResult = ActionResult + AccMeasurements(Index)
                Next Index
                ActionResult = ActionResult / 10

                MeasurementId = 1
                ActualSubState = SubStateDisplayResult
            Endif
        Endif

        Call LcdUpdate()
    Endif
End Sub



Sub ActionInStrikeSpeed()
    Local Index As Integer
    Local TempStr As String * 5

    ' Change substate if one of the buttons were pressed
    If GetFlagModeButton() = 1 Then
        Select Case ActualSubState
            Case SubStateStrikeSpeedConfigShort
                ActualSubState = SubStateStrikeSpeedConfigLong

            Case SubStateStrikeSpeedConfigLong
                ActualSubState = SubStateBackToMenu

            Case SubStateStrikeSpeedRunShort
                Call DisableStriking()
                ActualSubState = SubStateStrikeSpeedConfigShort

            Case SubStateStrikeSpeedRunLong
                Call DisableStriking()
                ActualSubState = SubStateStrikeSpeedConfigLong

            Case SubStateBackToMenu
                ActualSubState = SubStateStrikeSpeedConfigShort

            Case SubStateDisplayResult
                Call ChangeState(StateMainMenu)
                Return
        End Select

        Call LcdUpdate()
    Endif

    If GetFlagSetButton() = 1 Then
        Select Case ActualSubState
            Case SubStateStrikeSpeedConfigShort
                StrikeCount = 0
                Call TimerMs(0)
                FlagTimer = 0
                Call EnableStriking()
                ActualSubState = SubStateStrikeSpeedRunShort

            Case SubStateStrikeSpeedConfigLong
                StrikeCount = 0
                Call TimerMs(0)
                FlagTimer = 0
                Call EnableStriking()
                ActualSubState = SubStateStrikeSpeedRunLong

            Case SubStateBackToMenu
                Call ChangeState(StateMainMenu)
                Return

            Case SubStateDisplayResult
                Call ChangeState(StateMainMenu)
                Return
        End Select

        Call LcdUpdate()
    Endif

    ' Check if there is an unprocessed strike
    If GetFlagStrike() = 1 Then
        ' Start the timer after the first strike
        If StrikeCount = 0 Then
            If ActualSubState = SubStateStrikeSpeedRunShort Then
                Call TimerMs(StrikeSpeedTimeShort)
            Elseif ActualSubState = SubStateStrikeSpeedRunLong Then
                Call TimerMs(StrikeSpeedTimeLong)
            Endif
        Endif
        
        Incr StrikeCount
        Call LcdUpdate()
    Endif

    ' Check if the timer has ended
    If GetFlagTimer() = 1 Then
        Call DisableStriking()
        Call BeepMsExtended(BeepTimeStop, BeepTimeDefaultOff, 2)
        ActionResult = StrikeCount
        ActualSubState = SubStateDisplayResult
        Call LcdUpdate()
    Endif
End Sub



Sub ActionInReflex()

    Local Index As Integer
    Local TempStr As String * 8

    ' Change substate if one of the buttons were pressed
    If GetFlagModeButton() = 1 Then
        Select Case ActualSubState
            Case SubStateReflexConfig
                ActualSubState = SubStateBackToMenu

            Case SubStateReflexRun
                ActualSubState = SubStateReflexConfig

            Case SubStateBackToMenu
                ActualSubState = SubStateReflexConfig

            Case SubStateDisplayResult
                Call ChangeState(StateMainMenu)
                Return
        End Select

        Call LcdUpdate()
    Endif

    If GetFlagSetButton() = 1 Then
        Select Case ActualSubState
            Case SubStateReflexConfig
                MeasurementId = 1
                StrikeCount = 0
                Call TimerMs(0)
                FlagTimer = 0
                ReflexTime = 0
                FlagReflexTimerStarted = 0
                Call DisableStriking()

                ActualSubState = SubStateReflexRun

            Case SubStateBackToMenu
                Call ChangeState(StateMainMenu)
                Return

            Case SubStateDisplayResult
                If MeasurementId = 11 Then
                    MeasurementId = 1
                Else
                    Incr MeasurementId
                Endif
        End Select

        Call LcdUpdate()
    Endif

    ' Check if there is an unprocessed strike
    If GetFlagStrike() = 1 Then
        Call DisableStriking()

        ReflexTime = CurrentTime - ReflexTime
        FlagReflexTimerStarted = 0
        Call TimerMs(0)

        AccMeasurements(MeasurementId) = ReflexTime
        Incr MeasurementId
        Incr StrikeCount

        If MeasurementId > 10 Then
            ActionResult = 0
            For Index = 1 to 10
                ActionResult = ActionResult + AccMeasurements(Index)
            Next Index
            ActionResult = ActionResult / 10
            MeasurementId = 1

            ActualSubState = SubStateDisplayResult
            Call BeepMsExtended(BeepTimeStop, BeepTimeDefaultOff, 2)
        Endif

        Call LcdUpdate()
    Endif

    ' Check if the timer has ended
    If GetFlagTimer() = 1 Then
        If FlagReflexTimerStarted = 1 Then
            FlagReflexTimerStarted = 2
            Call TimerMs(999)
            Call BeepMs(BeepTimeTimer)
            ReflexTime = CurrentTime
            Call EnableStriking()
        Elseif FlagReflexTimerStarted = 2 Then
            Call DisableStriking()
            FlagReflexTimerStarted = 0
            
            AccMeasurements(MeasurementId) = 999
            Incr MeasurementId
            Incr StrikeCount

            If MeasurementId > 10 Then
                ActionResult = 0
                For Index = 1 to 10
                    ActionResult = ActionResult + AccMeasurements(Index)
                Next Index
                ActionResult = ActionResult / 10
                MeasurementId = 1

                ActualSubState = SubStateDisplayResult
                Call BeepMsExtended(BeepTimeStop, BeepTimeDefaultOff, 2)
            Else
                Call BeepMsExtended(BeepTimeTimeoutOn, BeepTimeTimeoutOff, 2)
            Endif
            
            Call LcdUpdate()
        Endif
    Endif

    ' If we are in run mode and the timer has not been set, set it to a random time
    If ActualSubState = SubStateReflexRun And FlagReflexTimerStarted = 0 Then
        Call TimerMs(Rnd(ReflexStrikeRndTime) + ReflexStrikeMinTime)
        FlagReflexTimerStarted = 1
    Endif

End Sub



Sub ActionInTrainer()
    ' Change substate if one of the buttons were pressed
    If GetFlagModeButton() = 1 Then
        Select Case ActualSubState
            Case SubStateTrainerConfig
                If TrainerCountSetup = 0 Then
                    ActualSubState = SubStateBackToMenu
                Else
                    If TrainerStrikeCount < TrainerStrikeCountMax Then
                        TrainerStrikeCount = TrainerStrikeCount + TrainerStrikeCountInc
                    Else
                        TrainerStrikeCount = TrainerStrikeCountMin
                    Endif
                Endif

            Case SubStateTrainerRun
                TrainerCountSetup = 0
                ActualSubState = SubStateTrainerConfig

            Case SubStateBackToMenu
                ActualSubState = SubStateTrainerConfig

            Case SubStateDisplayResult
                Call ChangeState(StateMainMenu)
                Return
        End Select

        Call LcdUpdate()
    Endif

    If GetFlagSetButton() = 1 Then
        Select Case ActualSubState
            Case SubStateTrainerConfig
                If TrainerCountSetup = 0 Then
                    TrainerCountSetup = 1
                Else
                    StrikeCount = 0
                    Call EnableStriking()

                    ActualSubState = SubStateTrainerRun
                Endif

            Case SubStateBackToMenu
                Call ChangeState(StateMainMenu)
                Return

            Case SubStateDisplayResult
                Call ChangeState(StateMainMenu)
                Return

        End Select

        Call LcdUpdate()
    Endif

    ' Check if there is an unprocessed strike
    If GetFlagStrike() = 1 Then
        Incr StrikeCount

        If StrikeCount = TrainerStrikeCount Then
            Call DisableStriking()
            Call BeepMsExtended(BeepTimeStop, BeepTimeDefaultOff, 2)
            ActionResult = TrainerStrikeCount
            ActualSubState = SubStateDisplayResult
        Endif

        Call LcdUpdate()
    Endif
End Sub


'___________ Before state action handler subroutines ______________________________________________
Sub ActionBeforeMainMenu()

End Sub


Sub ActionBeforeStrikeForce()
    ' Set selected substate to default
    ActualSubState = SubStateStrikeForceConfigTen
End Sub

Sub ActionBeforeStrikeSpeed()
    ' Set selected substate to default
    ActualSubState = SubStateStrikeSpeedConfigShort
End Sub

Sub ActionBeforeReflex()
    ' Set selected substate to default
    ActualSubState = SubStateReflexConfig
End Sub

Sub ActionBeforeTrainer()
    ' Set selected substate to default
    ActualSubState = SubStateTrainerConfig
    TrainerCountSetup = 0
End Sub



'___________ Helper functions and subroutines _____________________________________________________

Sub EnableStriking()
    StrikeEnabled = 1
End Sub

Sub DisableStriking()
    StrikeEnabled = 0
End Sub

Sub EnableLcdBackLight()
    LcdBackLightTimer = LcdBackLightTimerStartValue
    Reset PinLcdBackLight
End Sub

Sub BeepMs(ByVal intervalOn As Word)
    BeepIntervalOn = intervalOn
    BeepIntervalOff = 0
    BeepCount = 1
    BeepState = BeepOff
End Sub

Sub BeepMsExtended(ByVal intervalOn As Word, ByVal intervalOff As Word, ByVal count As Word)
    BeepIntervalOn = intervalOn
    BeepIntervalOff = intervalOff
    BeepCount = count
    BeepState = BeepOff
End Sub


Sub TimerMs(ByVal timeMs As Word)
    CounterTimer = timeMs / TimerPeriodInMs
End Sub


Sub LcdNotice(ByVal msg As String, ByVal timeMs As Word)
    Cls                             ' Clear the display
    Home                            ' Return home

    Lcd msg                         ' Print the message
    Waitms timeMs                   ' Wait for the given amount of time

    Call LcdUpdate()                ' Restore the original display
End Sub


' Returns the resultant force from the 3 force axis components
Function GetResultantForce(ByVal x As Long, ByVal y As Long, ByVal z As Long) As Long
    Local sx As Single
    Local sy As Single
    Local sz As Single
    Local Result As Single

    sx = x * x
    sy = y * y
    sz = z * z

    Result = sx + sy
    Result = Result + sz
    Result = Sqr(Result)
    GetResultantForce = Result
End Function


' These functions return the actual state of the flags and then reset them
Function GetFlagModeButton() As Byte
    Local FlagState As Byte

    If FlagModeButton = 1 Then
       FlagState = 1
       FlagModeButton = 0
    Else
       FlagState = 0
    Endif
    GetFlagModeButton = FlagState
End Function

Function GetFlagSetButton() As Byte
    Local FlagState As Byte

    If FlagSetButton = 1 Then
       FlagState = 1
       FlagSetButton = 0
    Else
       FlagState = 0
    Endif
    GetFlagSetButton = FlagState
End Function

Function GetFlagStrike() As Byte
    Local FlagState As Byte

    If FlagStrike = 1 Then
       FlagState = 1
       FlagStrike = 0
    Else
       FlagState = 0
    Endif
    GetFlagStrike = FlagState
End Function

Function GetFlagTimer() As Byte
    Local FlagState As Byte

    If FlagTimer = 1 Then
       FlagState = 1
       FlagTimer = 0
    Else
       FlagState = 0
    Endif
    GetFlagTimer = FlagState
End Function

Function GetFlagBattery() As Byte
    Local FlagState As Byte

    If FlagBattery = 1 Then
       FlagState = 1
       FlagBattery = 0
    Else
       FlagState = 0
    Endif
    GetFlagBattery = FlagState
End Function


'___________ Timer interrupt service routine ______________________________________________________
TimerISR:

    ' Set up timer to have a 1ms period
    TIMER1 = TimerReloadValue

    ' Increase the current time
    CurrentTime = CurrentTime + 1


    ' Debouncing the MODE btn
    If BtnModeState = BtnUp Then                ' Mode button state is up
        If PinModeBtn = PinLow Then             ' If the mode button is pressed, increase the debounce cntr
            Incr BtnModeCntr
        Else                                    ' Else reset the counter to zero
            BtnModeCntr = 0
        Endif

        If BtnModeCntr >= DebounceLimit Then    ' Debounce complete, btn is pressed down so set the stable state to up and set btn flag
            BtnModeCntr = 0
            BtnModeState = BtnDown
            FlagModeButton = 1            
            Call EnableLcdBackLight()
            Call BeepMs(BeepTimeBtn)
        Endif
    Else                                        ' Mode button state is down
        If PinModeBtn = PinHigh Then            ' If the mode button is released, increase the debounce cntr
            Incr BtnModeCntr
        Else                                    ' Else reset the counter to zero
            BtnModeCntr = 0
        Endif

        If BtnModeCntr >= DebounceLimit Then    ' Debounce complete, btn is released so set the stable state to up
            BtnModeCntr = 0
            BtnModeState = BtnUp
        Endif
    Endif


    ' Debouncing the SET btn
    If BtnSetState = BtnUp Then                 ' Set button state is up
        If PinSetBtn = PinLow Then              ' If the set button is pressed, increase the debounce cntr
            Incr BtnSetCntr
        Else                                    ' Else reset the counter to zero
            BtnSetCntr = 0
        Endif

        If BtnSetCntr >= DebounceLimit Then     ' Debounce complete, btn is pressed down so set the stable state to up and set btn flag
            BtnSetCntr = 0
            BtnSetState = BtnDown
            FlagSetButton = 1
            Call EnableLcdBackLight()
            Call BeepMs(BeepTimeBtn)
        Endif
    Else                                        ' Set button state is down
        If PinSetBtn = PinHigh Then             ' If the set button is released, increase the debounce cntr
            Incr BtnSetCntr
        Else                                    ' Else reset the counter to zero
            BtnSetCntr = 0
        Endif

        If BtnSetCntr >= DebounceLimit Then     ' Debounce complete, btn is released so set the stable state to up
            BtnSetCntr = 0
            BtnSetState = BtnUp
        Endif
    Endif


    ' Beep sound
    If BeepCount > 0 And CounterBeep = 0 Then
        If BeepState = BeepOff Then
            CounterBeep = BeepIntervalOn
            BeepState = BeepOn
            Decr BeepCount
        Else
            If BeepIntervalOff > 0 Then
                CounterBeep = BeepIntervalOff
            Endif
            BeepState = BeepOff
        Endif
    Endif
    
    If CounterBeep > 0 Then
        Decr CounterBeep
        If BeepState = BeepOn Then
            Reset PinLed
            Reset PinBeep
        Endif
        
        If CounterBeep = 0 Then
            Set PinLed
            Set PinBeep
        Endif
    Endif


    ' Software Timer
    If CounterTimer > 0 Then
        Decr CounterTimer

        If CounterTimer = 0 Then
            FlagTimer = 1
        Endif
    Endif


    ' Battery Timer
    If BatteryTimer > 0 Then
        Decr BatteryTimer
    Else
        BatteryTimer = BatteryCheckInterval
        FlagBattery = 1
    Endif
    
    ' Lcd Backlight Timer
    If LcdBackLightTimer > 0 Then
        Decr LcdBackLightTimer
        If LcdBackLightTimer = 0 Then
            Set PinLcdBackLight
        Endif
    Endif
Return

AccelerometerISR:
    If FlagStrike = 0 Then
        If StrikeEnabled = 1 Then
            MaxAcc = 0
            CurrAcc = 0
            MeasurementCount = 0

            While MeasurementCount < 5
                 AccX = Getadc(4)
                 AccY = Getadc(5)
                 AccZ = Getadc(6)
                 CurrAcc = GetResultantForce(AccX, AccY, AccZ)
                 If CurrAcc > MaxAcc Then
                     MaxAcc = CurrAcc
                     MeasurementCount = 0
                 Else
                     Incr MeasurementCount
                 Endif
            Wend

            Set PinXyzReset
            Set PinPunch
            Waitms 1
            Reset PinXyzReset
            Reset PinPunch

            FlagStrike = 1            
            Call EnableLcdBackLight()
            BeepMs(BeepTimeStrike)
        Else
            Set PinXyzReset
            Waitms 1
            Reset PinXyzReset
        Endif
    Endif
Return