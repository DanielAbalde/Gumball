Imports System.Collections.Generic
Imports System.IO
Imports System.Linq
Imports System.Windows.Forms
Imports Grasshopper.Kernel
Imports Rhino.Display
Imports Rhino.Geometry


Public Class GumballComp
    Inherits GH_Component

    Public Sub New()
        MyBase.New("Gumball ", "Gumball", "Gumball for Grasshopper geometry", "Params", "Util")
        Me.ValuesChanged()
    End Sub

#Region "Overrides"
    Protected Overrides ReadOnly Property Icon() As System.Drawing.Bitmap
        Get
            Return My.Resources.GumballIcon
        End Get
    End Property

    Public Overrides ReadOnly Property ComponentGuid() As Guid
        Get
            Return New Guid("{7b5a45b5-5ecc-4e34-9dcf-bfdeb8cc8deb}")
        End Get
    End Property

    Protected Overrides Sub RegisterInputParams(pManager As GH_Component.GH_InputParamManager)
        pManager.AddGeometryParameter("Geometry", "G", "Geometry to add a gumball", GH_ParamAccess.list)
    End Sub

    Protected Overrides Sub RegisterOutputParams(pManager As GH_Component.GH_OutputParamManager)
        pManager.AddGeometryParameter("Geometry", "G", "Transformed geometry", GH_ParamAccess.list)
        pManager.AddTransformParameter("Transform", "X", "Transformation data", GH_ParamAccess.list)
    End Sub

    Public Overrides Sub CreateAttributes()
        m_attributes = New GumballCompAtt(Me)
    End Sub

    Public Overrides Sub RemovedFromDocument(document As GH_Document)
        If (MyGumball IsNot Nothing) Then MyGumball.Dispose()
    End Sub

    Public Overrides Sub MovedBetweenDocuments(oldDocument As GH_Document, newDocument As GH_Document)
        If (MyGumball IsNot Nothing) Then MyGumball.HideGumballs()
    End Sub

    Public Overrides Sub DocumentContextChanged(document As GH_Document, context As GH_DocumentContext)
        MyBase.DocumentContextChanged(document, context)
        If (context = GH_DocumentContext.Close) AndAlso (MyGumball IsNot Nothing) Then MyGumball.Dispose()
    End Sub

    Public Overrides Property Locked As Boolean
        Get
            Return MyBase.Locked
        End Get
        Set(value As Boolean)
            If (MyGumball IsNot Nothing) AndAlso (value) Then MyGumball.HideGumballs()
            MyBase.Locked = value
        End Set
    End Property

    Protected Overrides Sub AppendAdditionalComponentMenuItems(ByVal menu As Windows.Forms.ToolStripDropDown)

        Dim union As Windows.Forms.ToolStripMenuItem = Menu_AppendItem(menu, "Apply to all", AddressOf Me.Menu_ApplyToAll, True, Me.ModeValue(0) = 1)
        union.ToolTipText = "Performs transformation of a gumball to all geometry"

        Dim aling As Windows.Forms.ToolStripMenuItem = Menu_AppendItem(menu, "Align to geometry", AddressOf Me.Menu_AlingToGeometry, True, CBool(Me.ModeValue(2)))
        aling.ToolTipText = "Use a geometry to align gumballs"

        Dim reloc As Windows.Forms.ToolStripMenuItem = Menu_AppendItem(menu, "Relocate gumball", AddressOf Me.Menu_RelocateG, True, Me.ModeValue(0) = 2)
        reloc.ToolTipText = "Relocate gumball without affecting the geometry"

        Dim reset As Windows.Forms.ToolStripMenuItem = Menu_AppendItem(menu, "Reset gumball", AddressOf Me.Menu_Reset, True)
        reset.ToolTipText = "Restore gumball to world coordinates"

        Dim CC As Windows.Forms.ToolStripMenuItem = Menu_AppendItem(menu, "Clear cache", AddressOf Me.Menu_ClearCache, True)
        CC.ToolTipText = "Reset gumball and clear cache data"

        Menu_AppendSeparator(menu)

        Dim arrows As Windows.Forms.ToolStripItem = Menu_AppendItem(menu, "Only arrows", AddressOf Me.Menu_OnlyArrows, True, Me.ModeValue(1) = 1)
        arrows.ToolTipText = "Show only arrows"

        Dim free As Windows.Forms.ToolStripItem = Menu_AppendItem(menu, "Free translate", AddressOf Me.Menu_FreeTranslate, True, Me.ModeValue(1) = 2)
        free.ToolTipText = "Hide all and drag from gumball center"

        Menu_AppendSeparator(menu)

        Dim Rad As Windows.Forms.ToolStripMenuItem = Menu_AppendItem(menu, "Attributes", AddressOf Me.Menu_Attributes, True)
        Rad.ToolTipText = "Changes the gumball attributes"

    End Sub

    Protected Overrides Sub ValuesChanged()

        Select Case Me.ModeValue(0)
            Case 0
                Me.Message = String.Empty
            Case 1
                Me.Message = "Apply to all"
            Case 2
                Me.Message = "Relocate"
        End Select

        If (ChangeAttributes) Then

            Me.RecordUndoEvent("gumballattributes")

            Select Case Me.ModeValue(1)
                Case 0
                    GumballAttributes(0) = 1
                    GumballAttributes(1) = 1
                    GumballAttributes(2) = 2
                    GumballAttributes(3) = 1
                    GumballAttributes(4) = 1
                Case 1
                    GumballAttributes(0) = 1
                    GumballAttributes(1) = 0
                    GumballAttributes(2) = 2
                    GumballAttributes(3) = 0
                    GumballAttributes(4) = 0
                Case 2
                    GumballAttributes(0) = 0
                    GumballAttributes(1) = 0
                    GumballAttributes(2) = 2
                    GumballAttributes(3) = 0
                    GumballAttributes(4) = 0
            End Select

            If (MyGumball IsNot Nothing) Then MyGumball.ChangeAppearance()
            ChangeAttributes = False
        End If

        If (Me.ModeValue(2)) Then
            If (Me.Params.Input.Count = 1) Then
                Dim param As New Grasshopper.Kernel.Parameters.Param_Geometry
                param.Name = "Geometry to align"
                param.NickName = "A"
                param.Description = "Use a geometry to align gumballs."
                param.Access = GH_ParamAccess.item
                Params.RegisterInputParam(param)
                Params.OnParametersChanged()
            End If
        Else
            If (Me.Params.Input.Count = 2) Then
                Dim param As IGH_Param = Params.Input(1)
                param.RemoveAllSources()
                Params.UnregisterInputParameter(param)
                Params.OnParametersChanged()
            End If
        End If

        Me.ExpireSolution(True)
    End Sub
#End Region

#Region "Menu"
    Private Sub Menu_ApplyToAll()
        If (1 = Me.ModeValue(0)) Then
            Me.ModeValue(0) = 0
        Else
            Me.ModeValue(0) = 1
        End If
    End Sub

    Private Sub Menu_AlingToGeometry()
        Me.ModeValue(2) = CInt(Not CBool(Me.ModeValue(2)))
    End Sub

    Private Sub Menu_RelocateG()
        If (2 = Me.ModeValue(0)) Then
            Me.ModeValue(0) = 0
        Else
            Me.ModeValue(0) = 2
        End If
    End Sub

    Private Sub Menu_Attributes()
        If (AttForm Is Nothing) Then
            AttForm = New FormAttributes(Me)
        Else
            AttForm.Dispose()
        End If
    End Sub

    Private Sub Menu_OnlyArrows()
        If (1 = Me.ModeValue(1)) Then
            Me.ModeValue(1) = 0
        Else
            Me.ModeValue(1) = 1
        End If
    End Sub

    Private Sub Menu_FreeTranslate()
        If (2 = Me.ModeValue(1)) Then
            Me.ModeValue(1) = 0
        Else
            Me.ModeValue(1) = 2
        End If
    End Sub

    Private Sub Menu_Reset()
        If (MyGumball IsNot Nothing) Then MyGumball.RestoreGumball()
    End Sub

    Public Sub Menu_ClearCache()
        Me.Cache.Clear()
        If (MyGumball IsNot Nothing) Then MyGumball.Dispose()
        Me.MyGumball = Nothing
        Me.ClearData()
        Me.ExpireSolution(True)
    End Sub
#End Region

    Public Property ModeValue(ByVal index As Integer) As Integer
        'Gumball mode = 0
        'Gumball attributes = 1
        'Align gumball = 2
        Get
            Select Case index
                Case 0
                    Return Me.GetValue("mode", 0)
                Case 1
                    Return Me.GetValue("att", 0)
                Case 2
                    Return Me.GetValue("align", False)
                Case Else
                    Throw New ArgumentOutOfRangeException()
            End Select
        End Get
        Set(value As Integer)
            Select Case index
                Case 0
                    Me.SetValue("mode", value)
                Case 1
                    ChangeAttributes = True
                    Me.SetValue("att", value)
                Case 2
                    Me.SetValue("align", value)
                Case Else
                    Throw New ArgumentOutOfRangeException()
            End Select
        End Set
    End Property

    Public Property GumballAttributes(ByVal Index As Integer) As Integer
        'Translate = 0  
        'Planar translate = 1  
        'Free translate = 2  
        'Rotate = 3 a 
        'Scale = 4  
        'Radius = 5  
        'Head = 6  
        'Thickness = 7  
        'Plane size = 8  
        'Plane distance = 9 
        Get
            Return MyGumballAttributes(Index)
        End Get
        Set(value As Integer)
            MyGumballAttributes(Index) = value
        End Set
    End Property

    Public MyGumball As GhGumball
    Private Cache As New List(Of GeometryBase)
    Private MyGumballAttributes As Integer() = New Integer(9) {1, 1, 2, 1, 1, 50, 5, 2, 15, 35}
    Public AttForm As FormAttributes = Nothing
    Public ChangeAttributes As New Boolean

    Protected Overrides Sub SolveInstance(DA As IGH_DataAccess)

        Dim InputData As New List(Of GeometryBase)
        Dim Data As New List(Of Types.IGH_GeometricGoo)

        'Get input data.
        If Not (DA.GetDataList(0, Data)) Then Exit Sub

        'Get geometry to align.
        If (Me.Params.Input.Count = 2) AndAlso (MyGumball IsNot Nothing) Then
            If (Me.Params.Input(1).VolatileDataCount > 0) Then
                Dim gg As Types.IGH_GeometricGoo = Nothing
                DA.GetData(1, gg)
                Dim g As GeometryBase = Grasshopper.Kernel.GH_Convert.ToGeometryBase(gg).Duplicate()
                MyGumball.AlignToGeometry(g)
            End If

        End If

        'GeometryGoo to GeometryBase.
        For Each d As Types.IGH_GeometricGoo In Data
            Dim g As GeometryBase = Grasshopper.Kernel.GH_Convert.ToGeometryBase(d)
            If (g Is Nothing) Then Continue For
            InputData.Add(g)
        Next

        'Set cache.
        If (Cache.Count = 0) Then Cache = InputData

        'Test if new inputdata
        If Not (AreEquals(Cache, InputData)) Then Menu_ClearCache()

        'Create Gumball class.
        If (MyGumball Is Nothing) Then
            Dim inputDataFree As GeometryBase() = New GeometryBase(InputData.Count - 1) {}
            For i As Int32 = 0 To InputData.Count - 1
                inputDataFree(i) = InputData(i).Duplicate
                inputDataFree(i).MakeDeformable()
            Next
            MyGumball = New GhGumball(inputDataFree, Me)
        End If

        'Set output data.
        Data.Clear()
        For Each g As GeometryBase In MyGumball.Geometry
            Dim d As Types.IGH_GeometricGoo = GH_Convert.ToGeometricGoo(g)
            If (d Is Nothing) Then Continue For
            Data.Add(d)
        Next
        DA.SetDataList(0, Data)
        DA.SetDataList(1, MyGumball.Xform.ToList())


    End Sub

    Private Function AreEquals(ByVal A As List(Of GeometryBase), ByVal B As List(Of GeometryBase)) As Boolean
        If (A.Count <> B.Count) Then
            Return False
            Exit Function
        End If
        For i As Int32 = 0 To A.Count - 1
            If (A(i).ObjectType <> B(i).ObjectType) Then
                Return False
                Exit Function
            End If
            Select Case A(i).ObjectType
                Case Rhino.DocObjects.ObjectType.Point
                    Dim ptA As Rhino.Geometry.Point = DirectCast(A(i), Rhino.Geometry.Point)
                    Dim ptB As Rhino.Geometry.Point = DirectCast(B(i), Rhino.Geometry.Point)
                    If (New Point3d(Math.Round(ptA.Location.X, 4), Math.Round(ptA.Location.Y, 4), Math.Round(ptA.Location.Z, 4)) <>
            New Point3d(Math.Round(ptB.Location.X, 4), Math.Round(ptB.Location.Y, 4), Math.Round(ptB.Location.Z, 4))) Then
                        Return False
                        Exit Function
                    End If
                Case Rhino.DocObjects.ObjectType.Curve
                    Dim CrvA As Curve = DirectCast(A(i), Curve)
                    Dim CrvB As Curve = DirectCast(B(i), Curve)
                    If (CrvA.ObjectType <> CrvB.ObjectType) Then
                        Return False
                        Exit Function
                    End If
                    If (CrvA.GetLength() <> CrvB.GetLength()) Then
                        Return False
                        Exit Function
                    End If
                    If (CrvA.Degree <> CrvB.Degree) Or (CrvA.Dimension <> CrvB.Dimension) Or (CrvA.Domain <> CrvB.Domain) Or
                            (CrvA.IsClosed <> CrvB.IsClosed) Or (CrvA.IsPeriodic <> CrvB.IsPeriodic) Or (CrvA.SpanCount <> CrvB.SpanCount) Then
                        Return False
                        Exit Function
                    End If
                    Dim paramA As Double() = CrvA.DivideByCount(40, True)
                    Dim paramB As Double() = CrvA.DivideByCount(40, True)
                    For j As Int32 = 0 To paramA.Count - 1
                        If (paramA(j) <> paramB(j)) Then
                            Return False
                            Exit Function
                        End If
                    Next

                Case Rhino.DocObjects.ObjectType.Brep
                    Dim BrpA As Brep = DirectCast(A(i), Brep)
                    Dim BrpB As Brep = DirectCast(B(i), Brep)
                    If (BrpA.Vertices.Count <> BrpB.Vertices.Count) Then
                        Return False
                        Exit Function
                    End If
                    If (BrpA.Surfaces.Count <> BrpB.Surfaces.Count) Then
                        Return False
                        Exit Function
                    End If
                    If (BrpA.Edges.Count <> BrpB.Edges.Count) Then
                        Return False
                        Exit Function
                    End If
                    For j As Int32 = 0 To BrpA.Vertices.Count - 1
                        If (New Point3d(Math.Round(BrpA.Vertices(j).Location.X, 4), Math.Round(BrpA.Vertices(j).Location.Y, 4), Math.Round(BrpA.Vertices(j).Location.Z, 4)) <>
              New Point3d(Math.Round(BrpB.Vertices(j).Location.X, 4), Math.Round(BrpB.Vertices(j).Location.Y, 4), Math.Round(BrpB.Vertices(j).Location.Z, 4))) Then
                            Return False
                            Exit Function
                        End If
                    Next
                Case Rhino.DocObjects.ObjectType.Mesh
                    Dim mshA As Mesh = DirectCast(A(i), Mesh)
                    Dim mshB As Mesh = DirectCast(B(i), Mesh)
                    If (mshA.Vertices.Count <> mshB.Vertices.Count) Then
                        Return False
                        Exit Function
                    End If
                    If (mshA.Faces.Count <> mshB.Faces.Count) Then
                        Return False
                        Exit Function
                    End If
                    For j As Int32 = 0 To mshA.Vertices.Count - 1
                        If (New Point3d(Math.Round(mshA.Vertices(j).X, 4), Math.Round(mshA.Vertices(j).Y, 4), Math.Round(mshA.Vertices(j).Z, 4)) <>
              New Point3d(Math.Round(mshB.Vertices(j).X, 4), Math.Round(mshB.Vertices(j).Y, 4), Math.Round(mshB.Vertices(j).Z, 4))) Then
                            Return False
                            Exit Function
                        End If
                    Next
            End Select
        Next
        Return True
    End Function

#Region "Write/Read"
    Public Overrides Function Write(ByVal writer As GH_IO.Serialization.GH_IWriter) As Boolean
        Try
            If (MyGumball IsNot Nothing) Then MyGumball.GumballWriter(writer)
        Catch ex As Exception
            Rhino.RhinoApp.WriteLine("WRITER_COMP; " & ex.ToString())
        End Try
        Return MyBase.Write(writer)
    End Function

    Public Overrides Function Read(ByVal reader As GH_IO.Serialization.GH_IReader) As Boolean
        If (MyGumball IsNot Nothing) Then
            MyGumball.Dispose()
        End If
        Dim newgh As New GhGumball()
        Dim gb As GhGumball = newgh.GumballReader(reader)

        'Attributes.
        Try
            Dim att As GH_IO.Serialization.GH_Chunk = reader.FindChunk("gumballattributes")

            Dim att0 As GH_IO.Serialization.GH_Chunk = att.FindChunk("gumballattributes_values", 0)
            Me.GumballAttributes(0) = att0.GetInt32("GhAtt_Translate", 0)
            Me.GumballAttributes(1) = att0.GetInt32("GhAtt_PlanarTranslate", 1)
            Me.GumballAttributes(2) = att0.GetInt32("GhAtt_FreeTranslate", 2)
            Me.GumballAttributes(3) = att0.GetInt32("GhAtt_Rotate", 3)
            Me.GumballAttributes(4) = att0.GetInt32("GhAtt_Scale", 4)
            Me.GumballAttributes(5) = att0.GetInt32("GhAtt_Radius", 5)
            Me.GumballAttributes(6) = att0.GetInt32("GhAtt_ArrowHead", 6)
            Me.GumballAttributes(7) = att0.GetInt32("GhAtt_Thickness", 7)
            Me.GumballAttributes(8) = att0.GetInt32("GhAtt_PlaneSize", 8)
            Me.GumballAttributes(9) = att0.GetInt32("GhAtt_PlaneDistance", 9)

            Dim att1 As GH_IO.Serialization.GH_Chunk = att.FindChunk("gumballattributes_modes", 1)
            Me.ModeValue(0) = att1.GetInt32("valmode", 0)
            Me.ModeValue(1) = att1.GetInt32("attmode", 1)
            Me.ModeValue(2) = att1.GetBoolean("aligntogeometry", 2)

        Catch ex As Exception
            Rhino.RhinoApp.WriteLine("READ_COMP_Attributes; " & ex.ToString())
        End Try

        If (gb IsNot Nothing) Then
            MyGumball = New GhGumball(gb, Me)
            MyGumball.ChangeAppearance()
        End If

        Return MyBase.Read(reader)
    End Function
#End Region

End Class

Public Class GumballCompAtt
    Inherits Grasshopper.Kernel.Attributes.GH_ComponentAttributes

    Private MyOwner As GumballComp

    Sub New(owner As GumballComp)
        MyBase.New(owner)
        MyOwner = owner
    End Sub

    Public Overrides Property Selected As Boolean
        Get
            Return MyBase.Selected
        End Get

        Set(value As Boolean)

            If (MyOwner.MyGumball IsNot Nothing) Then

                If Not (MyOwner.Hidden) AndAlso Not (MyOwner.Locked) Then

                    If (MyBase.Selected) Then
                        If Not (value) Then
                            MyOwner.MyGumball.HideGumballs()
                        End If
                    Else
                        If (value) Then
                            MyOwner.MyGumball.ShowGumballs()
                        End If
                    End If

                Else
                    MyOwner.MyGumball.HideGumballs()
                End If
                If (MyOwner.Params.Input(0).VolatileData.DataCount = 0) Then MyOwner.MyGumball.HideGumballs()
            End If

            MyBase.Selected = value
        End Set
    End Property

End Class

Public Class GhGumball
    Public Geometry As GeometryBase()
    Public Xform As Grasshopper.Kernel.Types.GH_Transform()
    Public Conduits As Rhino.UI.Gumball.GumballDisplayConduit()
    Private Gumballs As Rhino.UI.Gumball.GumballObject()
    Private Appearances As Rhino.UI.Gumball.GumballAppearanceSettings()
    Private MyCallBack As CustomCallBack
    Public Count As Integer
    Public Component As GumballComp
    Private GeometrytoAlign As GeometryBase

    Sub New()
    End Sub

    Sub New(Geo As GeometryBase(), comp As GumballComp)
        Component = comp
        Geometry = Geo
        Count = Geo.Count
        Xform = New Grasshopper.Kernel.Types.GH_Transform(Count - 1) {}
        Conduits = New Rhino.UI.Gumball.GumballDisplayConduit(Count - 1) {}
        Gumballs = New Rhino.UI.Gumball.GumballObject(Count - 1) {}
        Appearances = New Rhino.UI.Gumball.GumballAppearanceSettings(Count - 1) {}
        MyCallBack = New CustomCallBack(Me)

        For i As Int32 = 0 To Count - 1
            Xform(i) = New Grasshopper.Kernel.Types.GH_Transform()

            'Gumball appearance.
            Dim app As New Rhino.UI.Gumball.GumballAppearanceSettings
            app.MenuEnabled = False
            If (comp Is Nothing) Then
                app.Radius = 50
            Else
                'Translate.
                If Me.Component.GumballAttributes(0) Then
                    app.TranslateXEnabled = True
                    app.TranslateYEnabled = True
                    app.TranslateZEnabled = True
                Else
                    app.TranslateXEnabled = False
                    app.TranslateYEnabled = False
                    app.TranslateZEnabled = False
                End If
                'Free translate.
                If (Me.Component.GumballAttributes(2)) Then
                    app.FreeTranslate = 2
                Else
                    app.FreeTranslate = 0
                End If
                'Rotate.
                If Me.Component.GumballAttributes(3) Then
                    app.RotateXEnabled = True
                    app.RotateYEnabled = True
                    app.RotateZEnabled = True
                Else
                    app.RotateXEnabled = False
                    app.RotateYEnabled = False
                    app.RotateZEnabled = False
                End If
                'Scale.
                If Me.Component.GumballAttributes(4) Then
                    app.ScaleXEnabled = True
                    app.ScaleYEnabled = True
                    app.ScaleZEnabled = True
                Else
                    app.ScaleXEnabled = False
                    app.ScaleYEnabled = False
                    app.ScaleZEnabled = False
                End If
                'Radius.
                app.Radius = Me.Component.GumballAttributes(5)
                'Head.
                app.ArrowHeadLength = Me.Component.GumballAttributes(6) * 2
                app.ArrowHeadWidth = Me.Component.GumballAttributes(6)
                'Thickness.
                app.AxisThickness = Me.Component.GumballAttributes(7)
                app.ArcThickness = Me.Component.GumballAttributes(7)
                'Planar translate.
                If Me.Component.GumballAttributes(1) Then
                    app.TranslateXYEnabled = True
                    app.TranslateYZEnabled = True
                    app.TranslateZXEnabled = True
                    'Plane size.
                    app.PlanarTranslationGripSize = Me.Component.GumballAttributes(8)
                    'Plane distance.
                    app.PlanarTranslationGripCorner = Me.Component.GumballAttributes(9)
                Else
                    app.TranslateXYEnabled = False
                    app.TranslateYZEnabled = False
                    app.TranslateZXEnabled = False
                    'Plane size.
                    app.PlanarTranslationGripSize = 0
                End If
            End If
            If (Geometry(i).ObjectType = Rhino.DocObjects.ObjectType.Point) Then
                app.ScaleXEnabled = False
                app.ScaleYEnabled = False
                app.ScaleZEnabled = False
            End If
            Me.Appearances(i) = app

            'Gumball object.
            Dim GumBall As New Rhino.UI.Gumball.GumballObject
            If (Geo(i).ObjectType = Rhino.DocObjects.ObjectType.Point) Then
                Dim pt As Rhino.Geometry.Point = DirectCast(Geo(i), Rhino.Geometry.Point)
                GumBall.SetFromPlane(New Plane(pt.Location, Vector3d.XAxis, Vector3d.YAxis))
            ElseIf (Geo(i).ObjectType = Rhino.DocObjects.ObjectType.Curve) Then
                Dim crv As Rhino.Geometry.Curve = DirectCast(Geo(i), Rhino.Geometry.Curve)
                GumBall.SetFromCurve(crv)
            Else
                GumBall.SetFromBoundingBox(Geo(i).GetBoundingBox(True))
            End If
            Me.Gumballs(i) = GumBall

            'Display conduit.
            Dim conduit As New Rhino.UI.Gumball.GumballDisplayConduit
            conduit.SetBaseGumball(GumBall, app)
            Me.Conduits(i) = conduit
        Next

    End Sub

    Sub New(Other As GhGumball, comp As GumballComp)
        Geometry = Other.Geometry
        Xform = Other.Xform
        Conduits = Other.Conduits
        Gumballs = Other.Gumballs
        Appearances = Other.Appearances
        Count = Other.Geometry.Count
        MyCallBack = New CustomCallBack(Me)
        Component = comp
        For i As Int32 = 0 To Count - 1
            Conduits(i).SetBaseGumball(Other.Gumballs(i), Other.Appearances(i))
        Next
    End Sub

    Public Sub ShowGumballs()
        For i As Int32 = 0 To Count - 1
            Conduits(i).Enabled = True
        Next
        MyCallBack.Enabled = True
        Rhino.RhinoDoc.ActiveDoc.Views.Redraw()
    End Sub

    Public Sub HideGumballs()
        For i As Int32 = 0 To Count - 1
            Conduits(i).Enabled = False
        Next
        MyCallBack.Enabled = False
        Rhino.RhinoDoc.ActiveDoc.Views.Redraw()
    End Sub

    Public Sub Dispose()
        For i As Int32 = 0 To Count - 1
            Conduits(i).Enabled = False
            Conduits(i).Dispose()
            Gumballs(i).Dispose()
        Next
        MyCallBack.Enabled = False
    End Sub

    Public Sub UpdateGumball(ByVal Index As Integer)
        If Not (Conduits(Index).InRelocate) Then
            Dim xform As Transform = Conduits(Index).TotalTransform
            Conduits(Index).PreTransform = xform
        End If
        Dim gbframe As Rhino.UI.Gumball.GumballFrame = Conduits(Index).Gumball.Frame
        Dim baseFrame As Rhino.UI.Gumball.GumballFrame = Gumballs(Index).Frame
        baseFrame.Plane = gbframe.Plane
        baseFrame.ScaleGripDistance = gbframe.ScaleGripDistance
        Gumballs(Index).Frame = baseFrame
        Conduits(Index).SetBaseGumball(Gumballs(Index), Appearances(Index))
        Conduits(Index).Enabled = True

        Rhino.RhinoDoc.ActiveDoc.Views.Redraw()

        If (Me.Component.ModeValue(2)) AndAlso (GeometrytoAlign IsNot Nothing) Then AlignToGeometry(GeometrytoAlign)

    End Sub

    Public Sub UpdateGumball(ByVal i As Integer, xform As Transform)

        Dim gbframe As Rhino.UI.Gumball.GumballFrame = Conduits(i).Gumball.Frame
        Dim baseFrame As Rhino.UI.Gumball.GumballFrame = Gumballs(i).Frame
        Dim pln As Plane = gbframe.Plane
        pln.Transform(xform)
        baseFrame.Plane = pln
        baseFrame.ScaleGripDistance = gbframe.ScaleGripDistance
        Gumballs(i).Frame = baseFrame
        Conduits(i).SetBaseGumball(Gumballs(i), Appearances(i))
        Conduits(i).Enabled = True

        Rhino.RhinoDoc.ActiveDoc.Views.Redraw()

        If (Me.Component.ModeValue(2)) AndAlso (GeometrytoAlign IsNot Nothing) Then AlignToGeometry(GeometrytoAlign)

    End Sub

    Public Sub ChangeAppearance()

        For i As Int32 = 0 To Count - 1
            Dim app As Rhino.UI.Gumball.GumballAppearanceSettings = Appearances(i)

            'Translate.
            If Me.Component.GumballAttributes(0) Then
                app.TranslateXEnabled = True
                app.TranslateYEnabled = True
                app.TranslateZEnabled = True
            Else
                app.TranslateXEnabled = False
                app.TranslateYEnabled = False
                app.TranslateZEnabled = False
            End If
            'Free translate.
            If (Me.Component.GumballAttributes(2)) Then
                app.FreeTranslate = 2
            Else
                app.FreeTranslate = 0
            End If
            'Rotate.
            If Me.Component.GumballAttributes(3) Then
                app.RotateXEnabled = True
                app.RotateYEnabled = True
                app.RotateZEnabled = True
            Else
                app.RotateXEnabled = False
                app.RotateYEnabled = False
                app.RotateZEnabled = False
            End If
            'Scale.
            If Me.Component.GumballAttributes(4) Then
                app.ScaleXEnabled = True
                app.ScaleYEnabled = True
                app.ScaleZEnabled = True
            Else
                app.ScaleXEnabled = False
                app.ScaleYEnabled = False
                app.ScaleZEnabled = False
            End If
            'Radius.
            app.Radius = Me.Component.GumballAttributes(5)
            'Head.
            app.ArrowHeadLength = Me.Component.GumballAttributes(6) * 2
            app.ArrowHeadWidth = Me.Component.GumballAttributes(6)
            'Thickness.
            app.AxisThickness = Me.Component.GumballAttributes(7)
            app.ArcThickness = Me.Component.GumballAttributes(7)
            'Planar translate.
            If Me.Component.GumballAttributes(1) Then
                app.TranslateXYEnabled = True
                app.TranslateYZEnabled = True
                app.TranslateZXEnabled = True
                'Plane size.
                app.PlanarTranslationGripSize = Me.Component.GumballAttributes(8)
                'Plane distance.
                app.PlanarTranslationGripCorner = Me.Component.GumballAttributes(9)
            Else
                app.TranslateXYEnabled = False
                app.TranslateYZEnabled = False
                app.TranslateZXEnabled = False
                'Plane size.
                app.PlanarTranslationGripSize = 0
            End If


            If (Geometry(i).ObjectType = Rhino.DocObjects.ObjectType.Point) Then
                app.ScaleXEnabled = False
                app.ScaleYEnabled = False
                app.ScaleZEnabled = False
            End If

            Conduits(i).Enabled = False
            Appearances(i) = app
            Conduits(i).SetBaseGumball(Gumballs(i), Appearances(i))
            Conduits(i).Enabled = True
        Next
        Rhino.RhinoDoc.ActiveDoc.Views.Redraw()
    End Sub

    Public Sub AlignToGeometry(Geo As GeometryBase)
        GeometrytoAlign = Geo

        For i As Int32 = 0 To Count - 1

            Dim gbframe As Rhino.UI.Gumball.GumballFrame = Conduits(i).Gumball.Frame
            Dim baseFrame As Rhino.UI.Gumball.GumballFrame = Gumballs(i).Frame

            If (Geo.ObjectType = Rhino.DocObjects.ObjectType.Brep) Then
                Dim brp As Brep = DirectCast(Geo, Brep)
                Dim pln As Plane = gbframe.Plane
                Dim cpt As New Point3d
                Dim ci As ComponentIndex
                Dim normal As New Vector3d
                brp.ClosestPoint(gbframe.Plane.Origin, cpt, ci, Nothing, Nothing, 0, normal)
                If Not (ci.ComponentIndexType = ComponentIndexType.BrepFace) Or Not (normal.IsValid) Then
                    normal = New Vector3d(pln.Origin - cpt)
                End If
                Dim transform As Transform = Transform.Rotation(pln.ZAxis, normal, pln.Origin)
                pln.Transform(transform)
                gbframe.Plane = pln

            ElseIf (Geo.ObjectType = Rhino.DocObjects.ObjectType.Mesh) Then
                Dim msh As Mesh = DirectCast(Geo, Mesh)
                Dim mshpt As MeshPoint = msh.ClosestMeshPoint(gbframe.Plane.Origin, 0.0)
                Dim pln As Plane = gbframe.Plane
                Dim transform As Transform = Transform.Rotation(pln.ZAxis, msh.NormalAt(mshpt), pln.Origin)
                pln.Transform(transform)
                gbframe.Plane = pln

            ElseIf (Geo.ObjectType = Rhino.DocObjects.ObjectType.Curve) Then
                Dim crv As Curve = DirectCast(Geo, Curve)
                Dim t As New Double
                crv.ClosestPoint(gbframe.Plane.Origin, t)
                Dim pln As New Plane(gbframe.Plane.Origin, crv.TangentAt(t), Vector3d.CrossProduct(New Vector3d(gbframe.Plane.Origin - crv.PointAt(t)), crv.TangentAt(t)))
                gbframe.Plane = pln

            ElseIf (Geo.ObjectType = Rhino.DocObjects.ObjectType.Point) Then
                Dim pt As Rhino.Geometry.Point = DirectCast(Geo, Rhino.Geometry.Point)
                Dim pln As Plane = gbframe.Plane
                Dim transform As Transform = Transform.Rotation(pln.ZAxis, New Vector3d(pln.Origin - pt.Location), pln.Origin)
                pln.Transform(transform)
                gbframe.Plane = pln

            Else
                Continue For
            End If

            baseFrame.Plane = gbframe.Plane
            Gumballs(i).Frame = baseFrame
            Conduits(i).SetBaseGumball(Gumballs(i), Appearances(i))
            Conduits(i).Enabled = True

        Next
        Rhino.RhinoDoc.ActiveDoc.Views.Redraw()
    End Sub

    Public Sub RestoreGumball()
        For i As Int32 = 0 To Count - 1
            Dim gbframe As Rhino.UI.Gumball.GumballFrame = Conduits(i).Gumball.Frame
            Dim baseFrame As Rhino.UI.Gumball.GumballFrame = Gumballs(i).Frame
            gbframe.Plane = New Plane(gbframe.Plane.Origin, Vector3d.XAxis, Vector3d.YAxis)
            baseFrame.Plane = gbframe.Plane
            Gumballs(i).Frame = baseFrame
            Conduits(i).SetBaseGumball(Gumballs(i), Appearances(i))
            Conduits(i).Enabled = True
        Next
        Rhino.RhinoDoc.ActiveDoc.Views.Redraw()
    End Sub

#Region "Write/Read"
    Private gbd As String = "gumballdata"
    Private gge As String = "gumballgeometry"
    Private gxf As String = "gumballtransform"
    Private ggo As String = "gumballobject"
    Private gba As String = "gumballattributes"

    Public Function GumballWriter(ByVal writer As GH_IO.Serialization.GH_IWriter) As Boolean

        Try

            Dim i As New Integer
            writer.RemoveChunk(gbd)
            'Gumball writter.
            Dim alldata As GH_IO.Serialization.GH_IWriter = writer.CreateChunk(gbd)
            'Count.
            Dim c As GH_IO.Serialization.GH_IWriter = alldata.CreateChunk("count")
            c.SetInt32("size", 0, Me.Count)
            'Geometry.
            Dim geo As GH_IO.Serialization.GH_IWriter = alldata.CreateChunk(gge)
            For i = 0 To Count - 1
                Dim bytes As Byte() = GH_Convert.CommonObjectToByteArray(Geometry(i))
                geo.SetByteArray("geo", i, bytes)
            Next
            'Transform.
            Dim xf As GH_IO.Serialization.GH_IWriter = alldata.CreateChunk(gxf)
            For i = 0 To Count - 1
                Dim t As GH_IO.Serialization.GH_IWriter = xf.CreateChunk("xform", i)
                Xform(i).Write(t)
            Next
            'Gumball.
            Dim obj As GH_IO.Serialization.GH_IWriter = alldata.CreateChunk(ggo)
            For i = 0 To Count - 1
                Dim frame As Plane = Gumballs(i).Frame.Plane
                Dim pln As GH_IO.Types.GH_Plane
                pln.Origin = New GH_IO.Types.GH_Point3D(frame.Origin.X, frame.Origin.Y, frame.Origin.Z)
                pln.XAxis = New GH_IO.Types.GH_Point3D(frame.XAxis.X, frame.XAxis.Y, frame.XAxis.Z)
                pln.YAxis = New GH_IO.Types.GH_Point3D(frame.YAxis.X, frame.YAxis.Y, frame.YAxis.Z)
                obj.SetPlane("pln", i, pln)
                Dim scd As Vector3d = Gumballs(i).Frame.ScaleGripDistance
                Dim vec As New GH_IO.Types.GH_Point3D(scd.X, scd.Y, scd.Z)
                obj.SetPoint3D("scd", i, vec)
            Next
            'Attribute.
            writer.RemoveChunk("gumballattributes")
            Dim att As GH_IO.Serialization.GH_IWriter = writer.CreateChunk("gumballattributes")
            Dim att0 As GH_IO.Serialization.GH_IWriter = att.CreateChunk("gumballattributes_values", 0)
            att0.SetInt32("GhAtt_Translate", 0, Me.Component.GumballAttributes(0))
            att0.SetInt32("GhAtt_PlanarTranslate", 1, Me.Component.GumballAttributes(1))
            att0.SetInt32("GhAtt_FreeTranslate", 2, Me.Component.GumballAttributes(2))
            att0.SetInt32("GhAtt_Rotate", 3, Me.Component.GumballAttributes(3))
            att0.SetInt32("GhAtt_Scale", 4, Me.Component.GumballAttributes(4))
            att0.SetInt32("GhAtt_Radius", 5, Me.Component.GumballAttributes(5))
            att0.SetInt32("GhAtt_ArrowHead", 6, Me.Component.GumballAttributes(6))
            att0.SetInt32("GhAtt_Thickness", 7, Me.Component.GumballAttributes(7))
            att0.SetInt32("GhAtt_PlaneSize", 8, Me.Component.GumballAttributes(8))
            att0.SetInt32("GhAtt_PlaneDistance", 9, Me.Component.GumballAttributes(9))

            Dim att1 As GH_IO.Serialization.GH_IWriter = att.CreateChunk("gumballattributes_modes", 1)
            att1.SetInt32("valmode", 0, Me.Component.ModeValue(0))
            att1.SetInt32("attmode", 1, Me.Component.ModeValue(1))
            att1.SetBoolean("aligntogeometry", 2, Me.Component.ModeValue(2))

        Catch ex As Exception
            Rhino.RhinoApp.WriteLine("WRITER_GB; " & ex.ToString())
        End Try
        Return True
    End Function

    Public Function GumballReader(ByVal reader As GH_IO.Serialization.GH_IReader) As GhGumball

        If Not (reader.ChunkExists(gbd)) Then
            Return Nothing
            Exit Function
        End If

        Try
            Dim i As New Integer
            'Gumball reader.
            Dim alldata As GH_IO.Serialization.GH_IReader = reader.FindChunk(gbd)
            'Count.
            Dim c As GH_IO.Serialization.GH_IReader = alldata.FindChunk("count")
            Dim rcount As Integer = c.GetInt32("size", 0)
            'Geometry.
            Dim Geom As GeometryBase() = New GeometryBase(rcount - 1) {}
            Dim g As GH_IO.Serialization.GH_IReader = alldata.FindChunk(gge)
            For i = 0 To rcount - 1
                Dim bytes As Byte() = g.GetByteArray("geo", i)
                Geom(i) = GH_Convert.ByteArrayToCommonObject(Of GeometryBase)(bytes)
            Next
            'Transform.
            Dim transforms As Types.GH_Transform() = New Types.GH_Transform(rcount - 1) {}
            Dim xf As GH_IO.Serialization.GH_IReader = alldata.FindChunk(gxf)
            For i = 0 To rcount - 1
                Dim t As GH_IO.Serialization.GH_IReader = xf.FindChunk("xform", i)
                Dim xform As New Types.GH_Transform()
                xform.Read(t)
                transforms(i) = xform
            Next
            'Gumball.
            Dim gumobj As Rhino.UI.Gumball.GumballObject() = New Rhino.UI.Gumball.GumballObject(rcount - 1) {}
            Dim go As GH_IO.Serialization.GH_IReader = alldata.FindChunk(ggo)
            For i = 0 To rcount - 1
                Dim gb As New Rhino.UI.Gumball.GumballObject
                Dim frame As New Rhino.UI.Gumball.GumballFrame
                Dim pln As GH_IO.Types.GH_Plane = go.GetPlane("pln", i)
                frame.Plane = New Plane(New Point3d(pln.Origin.x, pln.Origin.y, pln.Origin.z), New Vector3d(pln.XAxis.x, pln.XAxis.y, pln.XAxis.z), New Vector3d(pln.YAxis.x, pln.YAxis.y, pln.YAxis.z))
                Dim scd As GH_IO.Types.GH_Point3D = go.GetPoint3D("scd", i)
                frame.ScaleGripDistance = New Vector3d(scd.x, scd.y, scd.z)
                gb.Frame = frame
                gumobj(i) = gb
            Next

            Dim readergumball As New GhGumball(Geom, Me.Component)
            readergumball.Xform = transforms
            readergumball.Gumballs = gumobj
            Return readergumball
            Exit Function
        Catch ex As Exception
            Rhino.RhinoApp.WriteLine("READER_GB; " & ex.ToString())
        End Try
        Return Nothing
    End Function
#End Region

End Class

Public Class CustomCallBack
    Inherits Rhino.UI.MouseCallback

    Private G As GhGumball
    Private Index As Integer
    Private Undo As Boolean

    Sub New(GHG As GhGumball)
        G = GHG
        Index = -1
        Undo = False
    End Sub

    Protected Overrides Sub OnMouseDown(e As Rhino.UI.MouseCallbackEventArgs)
        MyBase.OnMouseDown(e)
        Index = -1
        If (e.Button <> MouseButtons.Left) Then Exit Sub

        Dim Pick As New Rhino.Input.Custom.PickContext
        Pick.View = e.View
        Pick.PickStyle = Rhino.Input.Custom.PickStyle.PointPick
        Pick.SetPickTransform(e.View.ActiveViewport.GetPickTransform(e.ViewportPoint))
        Dim pickline As Line = Nothing
        e.View.ActiveViewport.GetFrustumLine(CDbl(e.ViewportPoint.X), CDbl(e.ViewportPoint.Y), pickline)
        Pick.PickLine = pickline
        Pick.UpdateClippingPlanes()

        For i As Int32 = 0 To G.Count - 1
            If (G.Conduits(i).PickGumball(Pick, Nothing)) Then
                Index = i
                Undo = True
                e.Cancel = True
                Exit For
            End If
        Next

    End Sub

    Protected Overrides Sub OnMouseMove(e As Rhino.UI.MouseCallbackEventArgs)
        MyBase.OnMouseMove(e)

        If (Index = -1) Or (Index >= G.Count) Then Exit Sub

        If (Undo) Then
            G.Component.RecordUndoEvent("gumballdata")
            Undo = False
        End If

        Dim conduit As Rhino.UI.Gumball.GumballDisplayConduit = G.Conduits(Index)
        If (conduit.PickResult.Mode = Rhino.UI.Gumball.GumballMode.None) Then Exit Sub
        conduit.CheckShiftAndControlKeys()
        Dim wordline As Line = Nothing
        If Not (e.View.MainViewport.GetFrustumLine(CDbl(e.ViewportPoint.X), CDbl(e.ViewportPoint.Y), wordline)) Then
            wordline = Line.Unset
        End If
        Dim cplane As Plane = e.View.MainViewport.GetConstructionPlane().Plane()
        Dim lp As Double = Nothing
        Rhino.Geometry.Intersect.Intersection.LinePlane(wordline, cplane, lp)
        Dim dragPoint As Point3d = wordline.PointAt(lp)
        If Not (conduit.UpdateGumball(dragPoint, wordline)) Then Exit Sub
        Rhino.RhinoDoc.ActiveDoc.Views.Redraw()
        e.Cancel = True
    End Sub

    Protected Overrides Sub OnMouseUp(e As Rhino.UI.MouseCallbackEventArgs)
        MyBase.OnMouseUp(e)
        If (Index = -1) Or (Index >= G.Count) Then Exit Sub

        Dim xform As Transform = G.Conduits(Index).GumballTransform

        If (xform <> Transform.Identity) Then

            If (G.Component.ModeValue(0) = 2) Then
                G.UpdateGumball(Index)
            Else

                Dim ghXform As New Grasshopper.Kernel.Types.GH_Transform(New Grasshopper.Kernel.Types.Transforms.Generic(xform))

                If (G.Component.ModeValue(0) = 1) Then

                    For i As Int32 = 0 To G.Count - 1
                        For Each t As Grasshopper.Kernel.Types.Transforms.ITransform In ghXform.CompoundTransforms
                            G.Xform(i).CompoundTransforms.Add(t.Duplicate())
                        Next
                        G.Xform(i).ClearCaches()
                        G.Geometry(i).Transform(xform)
                        If (i = Index) Then
                            G.UpdateGumball(i)
                        Else
                            G.UpdateGumball(i, xform)
                        End If
                    Next
                Else
                    For Each t As Grasshopper.Kernel.Types.Transforms.ITransform In ghXform.CompoundTransforms
                        G.Xform(Index).CompoundTransforms.Add(t.Duplicate())
                    Next
                    G.Xform(Index).ClearCaches()
                    G.Geometry(Index).Transform(xform)
                    G.UpdateGumball(Index)
                End If
            End If
            G.Component.ExpireSolution(True)
        End If
        Index = -1
        e.Cancel = True
    End Sub

End Class

Public Class FormAttributes
    Inherits System.Windows.Forms.Form

    Private Component As GumballComp
    Private CanSend As Boolean

    Sub New(Comp As GumballComp)
        Component = Comp
        InitializeComponent()
        Me.Show()
    End Sub

#Region "Events"
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        CanSend = False
        Me.ButtTranslate.Checked = Me.Component.GumballAttributes(0)
        Me.ButtPlane.Checked = Me.Component.GumballAttributes(1)
        Me.ButtFree.Checked = Me.Component.GumballAttributes(2)
        Me.ButtRotate.Checked = Me.Component.GumballAttributes(3)
        Me.ButtScale.Checked = Me.Component.GumballAttributes(4)
        Me.NumRad.Value = New Decimal(New Integer() {Me.Component.GumballAttributes(5), 0, 0, 0})
        Me.NumAH.Value = New Decimal(New Integer() {Me.Component.GumballAttributes(6), 0, 0, 0})
        Me.NumThk.Value = New Decimal(New Integer() {Me.Component.GumballAttributes(7), 0, 0, 0})
        Me.NumPS.Value = New Decimal(New Integer() {Me.Component.GumballAttributes(8), 0, 0, 0})
        Me.NumPD.Value = New Decimal(New Integer() {Me.Component.GumballAttributes(9), 0, 0, 0})
        CanSend = True
    End Sub

    Private Sub Form1_FormClosed(sender As Object, e As FormClosedEventArgs) Handles MyBase.FormClosed
        Me.Component.AttForm = Nothing
    End Sub

    Private Sub ButtTranslate_CheckedChanged(sender As Object, e As EventArgs) Handles ButtTranslate.CheckedChanged
        If (Me.Component.MyGumball IsNot Nothing) AndAlso (CanSend) Then
            Me.Component.GumballAttributes(0) = Me.ButtTranslate.CheckState.value__
            Me.Component.MyGumball.ChangeAppearance()
        End If
    End Sub

    Private Sub ButtPlane_CheckedChanged(sender As Object, e As EventArgs) Handles ButtPlane.CheckedChanged
        If (Me.Component.MyGumball IsNot Nothing) AndAlso (CanSend) Then
            Me.Component.GumballAttributes(1) = Me.ButtPlane.CheckState.value__
            Me.Component.MyGumball.ChangeAppearance()
        End If
    End Sub

    Private Sub ButtFree_CheckedChanged(sender As Object, e As EventArgs) Handles ButtFree.CheckedChanged
        If (Me.Component.MyGumball IsNot Nothing) AndAlso (CanSend) Then
            Me.Component.GumballAttributes(2) = Me.ButtFree.CheckState.value__
            Me.Component.MyGumball.ChangeAppearance()
        End If
    End Sub

    Private Sub ButtRotate_CheckedChanged(sender As Object, e As EventArgs) Handles ButtRotate.CheckedChanged
        If (Me.Component.MyGumball IsNot Nothing) AndAlso (CanSend) Then
            Me.Component.GumballAttributes(3) = Me.ButtRotate.CheckState.value__
            Me.Component.MyGumball.ChangeAppearance()
        End If
    End Sub

    Private Sub ButtScale_CheckedChanged(sender As Object, e As EventArgs) Handles ButtScale.CheckedChanged
        If (Me.Component.MyGumball IsNot Nothing) AndAlso (CanSend) Then
            Me.Component.GumballAttributes(4) = Me.ButtScale.CheckState.value__
            Me.Component.MyGumball.ChangeAppearance()
        End If
    End Sub

    Private Sub NumRad_ValueChanged(sender As Object, e As EventArgs) Handles NumRad.ValueChanged
        If (Me.Component.MyGumball IsNot Nothing) AndAlso (CanSend) Then
            Me.Component.GumballAttributes(5) = CInt(Me.NumRad.Value)
            Me.Component.MyGumball.ChangeAppearance()
        End If
    End Sub

    Private Sub NumAH_ValueChanged(sender As Object, e As EventArgs) Handles NumAH.ValueChanged
        If (Me.Component.MyGumball IsNot Nothing) AndAlso (CanSend) Then
            Me.Component.GumballAttributes(6) = CInt(Me.NumAH.Value)
            Me.Component.MyGumball.ChangeAppearance()
        End If
    End Sub

    Private Sub NumThk_ValueChanged(sender As Object, e As EventArgs) Handles NumThk.ValueChanged
        If (Me.Component.MyGumball IsNot Nothing) AndAlso (CanSend) Then
            Me.Component.GumballAttributes(7) = CInt(Me.NumThk.Value)
            Me.Component.MyGumball.ChangeAppearance()
        End If
    End Sub

    Private Sub NumPS_ValueChanged(sender As Object, e As EventArgs) Handles NumPS.ValueChanged
        If (Me.Component.MyGumball IsNot Nothing) AndAlso (CanSend) Then
            Me.Component.GumballAttributes(8) = CInt(Me.NumPS.Value)
            Me.Component.MyGumball.ChangeAppearance()
        End If
    End Sub

    Private Sub NumPD_ValueChanged(sender As Object, e As EventArgs) Handles NumPD.ValueChanged
        If (Me.Component.MyGumball IsNot Nothing) AndAlso (CanSend) Then
            Me.Component.GumballAttributes(9) = CInt(Me.NumPD.Value)
            Me.Component.MyGumball.ChangeAppearance()
        End If
    End Sub
#End Region

#Region "Design"
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    Private components As System.ComponentModel.IContainer

    Private Sub InitializeComponent()
        Me.NumRad = New System.Windows.Forms.NumericUpDown()
        Me.ButtTranslate = New System.Windows.Forms.CheckBox()
        Me.ButtPlane = New System.Windows.Forms.CheckBox()
        Me.ButtRotate = New System.Windows.Forms.CheckBox()
        Me.ButtScale = New System.Windows.Forms.CheckBox()
        Me.LabelRad = New System.Windows.Forms.Label()
        Me.NumAH = New System.Windows.Forms.NumericUpDown()
        Me.LabelArrow = New System.Windows.Forms.Label()
        Me.LabelThk = New System.Windows.Forms.Label()
        Me.NumThk = New System.Windows.Forms.NumericUpDown()
        Me.LabelPS = New System.Windows.Forms.Label()
        Me.NumPS = New System.Windows.Forms.NumericUpDown()
        Me.LabelPD = New System.Windows.Forms.Label()
        Me.NumPD = New System.Windows.Forms.NumericUpDown()
        Me.ButtFree = New System.Windows.Forms.CheckBox()
        CType(Me.NumRad, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.NumAH, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.NumThk, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.NumPS, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.NumPD, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'ButtTranslate
        '
        Me.ButtTranslate.CheckAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.ButtTranslate.Checked = True
        Me.ButtTranslate.CheckState = System.Windows.Forms.CheckState.Checked
        Me.ButtTranslate.Location = New System.Drawing.Point(12, 17)
        Me.ButtTranslate.Name = "ButtTranslate"
        Me.ButtTranslate.Size = New System.Drawing.Size(126, 17)
        Me.ButtTranslate.TabIndex = 1
        Me.ButtTranslate.Text = "Translate enabled"
        Me.ButtTranslate.UseVisualStyleBackColor = True
        '
        'ButtPlane
        '
        Me.ButtPlane.CheckAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.ButtPlane.Checked = True
        Me.ButtPlane.CheckState = System.Windows.Forms.CheckState.Checked
        Me.ButtPlane.Location = New System.Drawing.Point(12, 40)
        Me.ButtPlane.Name = "ButtPlane"
        Me.ButtPlane.Size = New System.Drawing.Size(126, 17)
        Me.ButtPlane.TabIndex = 2
        Me.ButtPlane.Text = "Plane enabled"
        Me.ButtPlane.UseVisualStyleBackColor = True
        '
        'ButtFree
        '
        Me.ButtFree.CheckAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.ButtFree.Checked = True
        Me.ButtFree.ThreeState = False
        Me.ButtFree.CheckState = System.Windows.Forms.CheckState.Checked
        Me.ButtFree.Location = New System.Drawing.Point(12, 63)
        Me.ButtFree.Name = "ButtFree"
        Me.ButtFree.Size = New System.Drawing.Size(126, 17)
        Me.ButtFree.TabIndex = 3
        Me.ButtFree.Text = "Free translate enabled"
        Me.ButtFree.UseVisualStyleBackColor = True
        '
        'ButtRotate
        '
        Me.ButtRotate.CheckAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.ButtRotate.Checked = True
        Me.ButtRotate.CheckState = System.Windows.Forms.CheckState.Checked
        Me.ButtRotate.Location = New System.Drawing.Point(12, 86)
        Me.ButtRotate.Name = "ButtRotate"
        Me.ButtRotate.Size = New System.Drawing.Size(126, 17)
        Me.ButtRotate.TabIndex = 4
        Me.ButtRotate.Text = "Rotate enabled"
        Me.ButtRotate.UseVisualStyleBackColor = True
        '
        'ButtScale
        '
        Me.ButtScale.CheckAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.ButtScale.Checked = True
        Me.ButtScale.CheckState = System.Windows.Forms.CheckState.Checked
        Me.ButtScale.Location = New System.Drawing.Point(12, 109)
        Me.ButtScale.Name = "ButtScale"
        Me.ButtScale.Size = New System.Drawing.Size(126, 17)
        Me.ButtScale.TabIndex = 5
        Me.ButtScale.Text = "Scale enabled"
        Me.ButtScale.UseVisualStyleBackColor = True
        '
        'NumRad
        '
        Me.NumRad.Location = New System.Drawing.Point(90, 132)
        Me.NumRad.Maximum = New Decimal(New Integer() {200, 0, 0, 0})
        Me.NumRad.Minimum = New Decimal(New Integer() {1, 0, 0, 0})
        Me.NumRad.Name = "NumRad"
        Me.NumRad.Size = New System.Drawing.Size(48, 20)
        Me.NumRad.TabIndex = 6
        Me.NumRad.Value = New Decimal(New Integer() {50, 0, 0, 0})
        '
        'NumAH
        '
        Me.NumAH.Location = New System.Drawing.Point(90, 158)
        Me.NumAH.Maximum = New Decimal(New Integer() {50, 0, 0, 0})
        Me.NumAH.Minimum = New Decimal(New Integer() {1, 0, 0, 0})
        Me.NumAH.Name = "NumAH"
        Me.NumAH.Size = New System.Drawing.Size(48, 20)
        Me.NumAH.TabIndex = 7
        Me.NumAH.Value = New Decimal(New Integer() {5, 0, 0, 0})
        '
        'NumThk
        '
        Me.NumThk.Location = New System.Drawing.Point(90, 184)
        Me.NumThk.Maximum = New Decimal(New Integer() {30, 0, 0, 0})
        Me.NumThk.Minimum = New Decimal(New Integer() {1, 0, 0, 0})
        Me.NumThk.Name = "NumThk"
        Me.NumThk.Size = New System.Drawing.Size(48, 20)
        Me.NumThk.TabIndex = 8
        Me.NumThk.Value = New Decimal(New Integer() {2, 0, 0, 0})
        '
        'NumPS
        '
        Me.NumPS.Location = New System.Drawing.Point(90, 210)
        Me.NumPS.Maximum = New Decimal(New Integer() {100, 0, 0, 0})
        Me.NumPS.Minimum = New Decimal(New Integer() {1, 0, 0, 0})
        Me.NumPS.Name = "NumPS"
        Me.NumPS.Size = New System.Drawing.Size(48, 20)
        Me.NumPS.TabIndex = 9
        Me.NumPS.Value = New Decimal(New Integer() {15, 0, 0, 0})
        '
        'NumPD
        '
        Me.NumPD.Location = New System.Drawing.Point(90, 236)
        Me.NumPD.Maximum = New Decimal(New Integer() {200, 0, 0, 0})
        Me.NumPD.Minimum = New Decimal(New Integer() {0, 0, 0, 0})
        Me.NumPD.Name = "NumPD"
        Me.NumPD.Size = New System.Drawing.Size(48, 20)
        Me.NumPD.TabIndex = 10
        Me.NumPD.Value = New Decimal(New Integer() {35, 0, 0, 0})
        '
        'LabelRad
        '
        Me.LabelRad.AutoSize = True
        Me.LabelRad.Location = New System.Drawing.Point(12, 134)
        Me.LabelRad.Name = "LabelRad"
        Me.LabelRad.Size = New System.Drawing.Size(40, 13)
        Me.LabelRad.TabIndex = 11
        Me.LabelRad.Text = "Radius"
        '
        'LabelArrow
        '
        Me.LabelArrow.AutoSize = True
        Me.LabelArrow.Location = New System.Drawing.Point(12, 160)
        Me.LabelArrow.Name = "LabelArrow"
        Me.LabelArrow.Size = New System.Drawing.Size(61, 13)
        Me.LabelArrow.TabIndex = 12
        Me.LabelArrow.Text = "Arrow head"
        '
        'LabelThk
        '
        Me.LabelThk.AutoSize = True
        Me.LabelThk.Location = New System.Drawing.Point(12, 186)
        Me.LabelThk.Name = "LabelThk"
        Me.LabelThk.Size = New System.Drawing.Size(56, 13)
        Me.LabelThk.TabIndex = 13
        Me.LabelThk.Text = "Thickness"
        '
        'LabelPS
        '
        Me.LabelPS.AutoSize = True
        Me.LabelPS.Location = New System.Drawing.Point(12, 212)
        Me.LabelPS.Name = "LabelPS"
        Me.LabelPS.Size = New System.Drawing.Size(55, 13)
        Me.LabelPS.TabIndex = 14
        Me.LabelPS.Text = "Plane size"
        '
        'LabelPD
        '
        Me.LabelPD.AutoSize = True
        Me.LabelPD.Location = New System.Drawing.Point(12, 238)
        Me.LabelPD.Name = "LabelPD"
        Me.LabelPD.Size = New System.Drawing.Size(77, 13)
        Me.LabelPD.TabIndex = 15
        Me.LabelPD.Text = "Plane distance"
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(153, 273)
        Me.Controls.Add(Me.ButtFree)
        Me.Controls.Add(Me.LabelPD)
        Me.Controls.Add(Me.NumPD)
        Me.Controls.Add(Me.LabelPS)
        Me.Controls.Add(Me.NumPS)
        Me.Controls.Add(Me.LabelThk)
        Me.Controls.Add(Me.NumThk)
        Me.Controls.Add(Me.LabelArrow)
        Me.Controls.Add(Me.NumAH)
        Me.Controls.Add(Me.LabelRad)
        Me.Controls.Add(Me.ButtScale)
        Me.Controls.Add(Me.ButtRotate)
        Me.Controls.Add(Me.ButtPlane)
        Me.Controls.Add(Me.ButtTranslate)
        Me.Controls.Add(Me.NumRad)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "Form1"
        Me.ShowIcon = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Owner = Grasshopper.Instances.DocumentEditor
        Me.Text = "Gumball Attributes"
        CType(Me.NumRad, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.NumAH, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.NumThk, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.NumPS, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.NumPD, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents NumRad As NumericUpDown
    Friend WithEvents ButtTranslate As CheckBox
    Friend WithEvents ButtPlane As CheckBox
    Friend WithEvents ButtRotate As CheckBox
    Friend WithEvents ButtScale As CheckBox
    Friend WithEvents LabelRad As Label
    Friend WithEvents NumAH As NumericUpDown
    Friend WithEvents LabelArrow As Label
    Friend WithEvents LabelThk As Label
    Friend WithEvents NumThk As NumericUpDown
    Friend WithEvents LabelPS As Label
    Friend WithEvents NumPS As NumericUpDown
    Friend WithEvents LabelPD As Label
    Friend WithEvents NumPD As NumericUpDown
    Friend WithEvents ButtFree As CheckBox
#End Region

End Class
