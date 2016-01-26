Imports System.Collections.Generic
Imports System.IO
Imports System.Linq
Imports System.Windows.Forms
Imports Grasshopper
Imports Grasshopper.Kernel
Imports GH_IO.Serialization
Imports Rhino.Display
Imports Rhino.Geometry
Imports Grasshopper.Kernel.Data
Imports GH_IO
Imports Grasshopper.Kernel.Undo

Public Class GumballComp
    Inherits GH_Component

    Public Sub New()
        MyBase.New("Gumball ", "Gumball", "Gumball for Grasshopper geometry", "Params", "Util")
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
        pManager.AddGeometryParameter("Geometry", "G", "Geometry to add a gumball", GH_ParamAccess.tree)
    End Sub

    Protected Overrides Sub RegisterOutputParams(pManager As GH_Component.GH_OutputParamManager)
        pManager.AddGeometryParameter("Geometry", "G", "Transformed geometry", GH_ParamAccess.tree)
        pManager.AddTransformParameter("Transform", "X", "Transformation data", GH_ParamAccess.tree)
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

    Protected Overrides Sub AfterSolveInstance()
        If (MyGumball IsNot Nothing) Then
            If (Me.Hidden) Then MyGumball.HideGumballs()
            If Not (Me.Attributes.Selected) Then MyGumball.HideGumballs()
        End If
    End Sub

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

#End Region

#Region "Menu"
    Private Sub Menu_ApplyToAll()
        If (MyGumball IsNot Nothing) Then Me.RecordUndoEvent("Gumball Mode", New GbUndo(Me.MyGumball))
        If (1 = Me.ModeValue(0)) Then
            Me.ModeValue(0) = 0
        Else
            Me.ModeValue(0) = 1
        End If
    End Sub

    Private Sub Menu_AlingToGeometry()
        If (MyGumball IsNot Nothing) Then Me.RecordUndoEvent("Gumball Mode", New GbUndo(Me.MyGumball))
        Me.ModeValue(2) = CInt(Not CBool(Me.ModeValue(2)))
    End Sub

    Private Sub Menu_RelocateG()
        If (MyGumball IsNot Nothing) Then Me.RecordUndoEvent("Gumball Mode", New GbUndo(Me.MyGumball))
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
        If (MyGumball Is Nothing) Then Exit Sub
        Me.RecordUndoEvent("Gumball Attributes", New GbUndo(Me.MyGumball))
        If (ModeValueAtt = 1) Then
            ModeValueAtt = 0
            MyGumball.CustomAppearance = New Integer(9) {1, 1, 2, 1, 1, MyGumball.CustomAppearance(5), MyGumball.CustomAppearance(6),
                MyGumball.CustomAppearance(7), MyGumball.CustomAppearance(8), MyGumball.CustomAppearance(9)}
        Else
            ModeValueAtt = 1
            MyGumball.CustomAppearance = New Integer(9) {1, 0, 2, 0, 0, MyGumball.CustomAppearance(5), MyGumball.CustomAppearance(6),
                MyGumball.CustomAppearance(7), MyGumball.CustomAppearance(8), MyGumball.CustomAppearance(9)}
        End If

    End Sub

    Private Sub Menu_FreeTranslate()
        If (MyGumball Is Nothing) Then Exit Sub
        Me.RecordUndoEvent("Gumball Attributes", New GbUndo(Me.MyGumball))

        If (ModeValueAtt = 2) Then
            ModeValueAtt = 0
            MyGumball.CustomAppearance = New Integer(9) {1, 1, 2, 1, 1, MyGumball.CustomAppearance(5), MyGumball.CustomAppearance(6),
                 MyGumball.CustomAppearance(7), MyGumball.CustomAppearance(8), MyGumball.CustomAppearance(9)}
        Else
            ModeValueAtt = 2
            MyGumball.CustomAppearance = New Integer(9) {0, 0, 2, 0, 0, MyGumball.CustomAppearance(5), MyGumball.CustomAppearance(6),
                MyGumball.CustomAppearance(7), MyGumball.CustomAppearance(8), MyGumball.CustomAppearance(9)}
        End If

    End Sub

    Private Sub Menu_Reset()
        Me.RecordUndoEvent("Gumball Reset")
        If (MyGumball IsNot Nothing) Then MyGumball.RestoreGumball()
    End Sub

    Public Sub Menu_ClearCache()
        Me.RecordUndoEvent("Gumball Clear Cache")
        Me.Cache.Clear()
        If (MyGumball IsNot Nothing) Then MyGumball.Dispose()
        Me.MyGumball = Nothing
        Me.ClearData()
        Me.ExpireSolution(True)
    End Sub
#End Region

    Private Sub EventAddParamAlign()

        If (Me.ModeValue(2)) Then
            If (Me.Params.Input.Count = 1) Then
                Dim param As New Grasshopper.Kernel.Parameters.Param_Geometry
                param.Optional = True
                param.Name = "Geometry to align"
                param.NickName = "A"
                param.Description = "Use a geometry to align gumballs."
                param.Access = GH_ParamAccess.item
                Params.RegisterInputParam(param)
                Params.OnParametersChanged()
            End If
        Else
            If (Me.Params.Input.Count = 2) Then
                MyGumball.GeometrytoAlign = Nothing
                Dim param As IGH_Param = Params.Input(1)
                param.RemoveAllSources()
                Params.UnregisterInputParameter(param)
                Params.OnParametersChanged()
            End If
        End If
        Me.ExpireSolution(True)
    End Sub

    Public Property ModeValue(ByVal index As Integer) As Integer
        'Gumball mode = 0
        'Gumball attributes = 1
        'Align gumball = 2
        Get
            Select Case index
                Case 0
                    Return ModeValueType
                Case 1
                    Return ModeValueAtt
                Case 2
                    Return ModeValueAlign
                Case Else
                    Throw New ArgumentOutOfRangeException()
            End Select
        End Get
        Set(value As Integer)

            Select Case index
                Case 0
                    ModeValueType = value

                    Select Case value
                        Case 0
                            Me.Message = String.Empty
                        Case 1
                            Me.Message = "Apply to all"
                        Case 2
                            Me.Message = "Relocate"
                    End Select

                    Me.ExpireSolution(True)
                Case 1
                    ModeValueAtt = value
                Case 2
                    ModeValueAlign = value
                    EventAddParamAlign()
                Case Else
                    Throw New ArgumentOutOfRangeException()
            End Select
        End Set
    End Property

    Public MyGumball As GhGumball
    Private Cache As New DataTree(Of GeometryBase)
    Private Paths As GH_Path()
    Public AttForm As FormAttributes = Nothing
    Private MyGumballAttributes As Integer() = New Integer(9) {1, 1, 2, 1, 1, 50, 5, 2, 15, 35}

    Private ModeValueType As New Integer
    Private ModeValueAlign As New Boolean
    Private ModeValueAtt As New Integer

    Protected Overrides Sub SolveInstance(DA As IGH_DataAccess)
        Dim Data As New GH_Structure(Of Types.IGH_GeometricGoo)
        Dim InputData As New DataTree(Of GeometryBase)

        'Get input data.
        If Not (DA.GetDataTree(0, Data)) Then Exit Sub

        'Get geometry to align.
        If (Me.Params.Input.Count = 2) AndAlso (MyGumball IsNot Nothing) Then
            If (Me.Params.Input(1).VolatileDataCount > 0) Then
                Dim gg As Types.IGH_GeometricGoo = Nothing
                DA.GetData(1, gg)
                Dim g As GeometryBase = Grasshopper.Kernel.GH_Convert.ToGeometryBase(gg).Duplicate()
                If ((MyGumball.GeometrytoAlign Is Nothing)) Then
                    MyGumball.AlignToGeometry(g)
                Else
                    Dim gtree As New DataTree(Of GeometryBase)
                    gtree.Add(g, New GH_Path(0))
                    Dim atree As New DataTree(Of GeometryBase)
                    atree.Add(MyGumball.GeometrytoAlign, New GH_Path(0))

                    If Not (AreEquals(gtree, atree)) Then
                        MyGumball.AlignToGeometry(g)
                        If (Attributes.Selected) Then MyGumball.ShowGumballs()
                    End If
                End If
            End If
            End If

        'GeometryGoo to GeometryBase.
        For Each b As GH_Path In Data.Paths
            For Each d As Types.IGH_GeometricGoo In Data.DataList(b)
                Dim g As GeometryBase = Grasshopper.Kernel.GH_Convert.ToGeometryBase(d).Duplicate()
                If (g Is Nothing) Then Continue For
                InputData.Add(g, b)
            Next
        Next

        'Set cache.
        If (Cache.DataCount = 0) Then
            SetCache(InputData)
        Else
            'Test if new inputdata
            If Not (AreEquals(Cache, InputData)) Then
                Cache.Clear()
                If (MyGumball IsNot Nothing) Then MyGumball.Dispose()
                MyGumball = Nothing
                SetCache(InputData)
            End If
        End If

        'Create Gumball class.
        If (MyGumball Is Nothing) Then
            Dim inputDataFree As GeometryBase() = New GeometryBase(InputData.DataCount - 1) {}
            Dim tolist As List(Of GeometryBase) = InputData.AllData()
            For i As Int32 = 0 To tolist.Count - 1
                inputDataFree(i) = tolist(i).Duplicate
                inputDataFree(i).MakeDeformable()
            Next
            MyGumball = New GhGumball(inputDataFree, Me)
            If (Me.Attributes.Selected) Then MyGumball.ShowGumballs()
        End If

        'Set output data.
        Dim DataOutput As New GH_Structure(Of Types.IGH_GeometricGoo)
        Dim DataOutput2 As New GH_Structure(Of Types.GH_Transform)
        If (Paths.Count > 0) Then
            For i As Int32 = 0 To MyGumball.Count - 1
                Dim d As Types.IGH_GeometricGoo = GH_Convert.ToGeometricGoo(MyGumball.Geometry(i))
                DataOutput.Append(d, Paths(i))
                DataOutput2.Append(MyGumball.Xform(i), Paths(i))
            Next
        End If

        DA.SetDataTree(0, DataOutput)
        DA.SetDataTree(1, DataOutput2)

    End Sub

    Private Sub SetCache(_InputData As DataTree(Of GeometryBase))

        Cache.Clear()
        Paths = New GH_Path(_InputData.DataCount - 1) {}
        Dim count As New Integer
        For i As Int32 = 0 To _InputData.BranchCount - 1
            For j As Int32 = 0 To _InputData.Branch(i).Count - 1
                Cache.Add(_InputData.Branch(i)(j).Duplicate(), _InputData.Path(i))
                Paths(count) = _InputData.Path(i)
                count += 1
            Next
        Next
    End Sub

    Private Function AreEquals(ByVal A As DataTree(Of GeometryBase), ByVal B As DataTree(Of GeometryBase)) As Boolean
        If (A.DataCount <> B.DataCount) Then
            ' Rhino.RhinoApp.WriteLine("Distinto DataCount")
            Return False
            Exit Function
        End If
        If (A.BranchCount <> B.BranchCount) Then
            ' Rhino.RhinoApp.WriteLine("Distinto BranchCount")
            Return False
            Exit Function
        End If
        For br As Int32 = 0 To A.BranchCount - 1
            For i As Int32 = 0 To A.Branch(br).Count - 1
                If (A.Path(br) <> B.Path(br)) Then
                    ' Rhino.RhinoApp.WriteLine("Distinto Path")
                    Return False
                    Exit Function
                End If
                If (A.Branch(br)(i).ObjectType <> B.Branch(br)(i).ObjectType) Then
                    ' Rhino.RhinoApp.WriteLine("Distinto ObjectType")
                    Return False
                    Exit Function
                End If
                Select Case A.Branch(br)(i).ObjectType
                    Case Rhino.DocObjects.ObjectType.Point
                        Dim ptA As Rhino.Geometry.Point = DirectCast(A.Branch(br)(i), Rhino.Geometry.Point)
                        Dim ptB As Rhino.Geometry.Point = DirectCast(B.Branch(br)(i), Rhino.Geometry.Point)
                        If (New Point3d(Math.Round(ptA.Location.X, 4), Math.Round(ptA.Location.Y, 4), Math.Round(ptA.Location.Z, 4)) <>
            New Point3d(Math.Round(ptB.Location.X, 4), Math.Round(ptB.Location.Y, 4), Math.Round(ptB.Location.Z, 4))) Then
                            Return False
                            Exit Function
                        End If
                    Case Rhino.DocObjects.ObjectType.Curve
                        Dim CrvA As Curve = DirectCast(A.Branch(br)(i), Curve)
                        Dim CrvB As Curve = DirectCast(B.Branch(br)(i), Curve)
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
                        Dim BrpA As Brep = DirectCast(A.Branch(br)(i), Brep)
                        Dim BrpB As Brep = DirectCast(B.Branch(br)(i), Brep)
                        If (BrpA.Vertices.Count <> BrpB.Vertices.Count) Then
                            '     Rhino.RhinoApp.WriteLine("Distinto VerticesCount")
                            Return False
                            Exit Function
                        End If
                        If (BrpA.Surfaces.Count <> BrpB.Surfaces.Count) Then
                            '    Rhino.RhinoApp.WriteLine("Distinto SrfCount")
                            Return False
                            Exit Function
                        End If
                        If (BrpA.Edges.Count <> BrpB.Edges.Count) Then
                            '   Rhino.RhinoApp.WriteLine("Distinto EdgeCount")
                            Return False
                            Exit Function
                        End If
                        For j As Int32 = 0 To BrpA.Vertices.Count - 1
                            If (New Point3d(Math.Round(BrpA.Vertices(j).Location.X, 4), Math.Round(BrpA.Vertices(j).Location.Y, 4), Math.Round(BrpA.Vertices(j).Location.Z, 4)) <>
              New Point3d(Math.Round(BrpB.Vertices(j).Location.X, 4), Math.Round(BrpB.Vertices(j).Location.Y, 4), Math.Round(BrpB.Vertices(j).Location.Z, 4))) Then
                                '     Rhino.RhinoApp.WriteLine("Distinto Vertices")
                                Return False
                                Exit Function
                            End If
                        Next
                    Case Rhino.DocObjects.ObjectType.Mesh
                        Dim mshA As Mesh = DirectCast(A.Branch(br)(i), Mesh)
                        Dim mshB As Mesh = DirectCast(B.Branch(br)(i), Mesh)
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
        Next
        Return True
    End Function

#Region "Write/Read"

    Public Overrides Function Write(ByVal writer As GH_IO.Serialization.GH_IWriter) As Boolean
        If (MyGumball IsNot Nothing) Then MyGumball.GumballWriter(writer)
        Return MyBase.Write(writer)
    End Function

    Public Overrides Function Read(ByVal reader As GH_IO.Serialization.GH_IReader) As Boolean

        If (MyGumball Is Nothing) Then
            MyGumball = New GhGumball(reader, Me)
        Else
            MyGumball.HideGumballs()
            MyGumball.GumballReader(reader)
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
    Inherits Rhino.UI.MouseCallback
    Implements GH_ISerializable

    Public Geometry As GeometryBase()
    Public Xform As Grasshopper.Kernel.Types.GH_Transform()

    Public Conduits As Rhino.UI.Gumball.GumballDisplayConduit()
    Private Gumballs As Rhino.UI.Gumball.GumballObject()
    Private Appearances As Rhino.UI.Gumball.GumballAppearanceSettings()
    Private MyCustomAppearance As Integer() = New Integer(9) {1, 1, 2, 1, 1, 50, 5, 2, 15, 35}

    Public Count As Integer
    Public Component As GumballComp
    Public GeometrytoAlign As GeometryBase

#Region "New/Show/Hide/Dispose"
    Sub New(Geo As GeometryBase(), comp As GumballComp)
        Component = comp
        Geometry = Geo
        Count = Geo.Count
        Xform = New Grasshopper.Kernel.Types.GH_Transform(Count - 1) {}
        Conduits = New Rhino.UI.Gumball.GumballDisplayConduit(Count - 1) {}
        Gumballs = New Rhino.UI.Gumball.GumballObject(Count - 1) {}
        Appearances = New Rhino.UI.Gumball.GumballAppearanceSettings(Count - 1) {}

        For i As Int32 = 0 To Count - 1
            Xform(i) = New Grasshopper.Kernel.Types.GH_Transform()

            'Appearance.
            Dim app As New Rhino.UI.Gumball.GumballAppearanceSettings
            app.MenuEnabled = False

            'Translate.
            app.TranslateXEnabled = MyCustomAppearance(0)
            app.TranslateYEnabled = MyCustomAppearance(0)
            app.TranslateZEnabled = MyCustomAppearance(0)
            'Free translate.
            If (MyCustomAppearance(2)) Then
                app.FreeTranslate = 2
            Else
                app.FreeTranslate = 0
            End If
            'Rotate.
            app.RotateXEnabled = MyCustomAppearance(3)
            app.RotateYEnabled = MyCustomAppearance(3)
            app.RotateZEnabled = MyCustomAppearance(3)
            'Scale.
            app.ScaleXEnabled = MyCustomAppearance(4)
            app.ScaleYEnabled = MyCustomAppearance(4)
            app.ScaleZEnabled = MyCustomAppearance(4)
            'Radius.
            app.Radius = MyCustomAppearance(5)
            'Head.
            app.ArrowHeadLength = MyCustomAppearance(6) * 2
            app.ArrowHeadWidth = MyCustomAppearance(6)
            'Thickness.
            app.AxisThickness = MyCustomAppearance(7)
            app.ArcThickness = MyCustomAppearance(7)
            'Planar translate.
            If MyCustomAppearance(1) Then
                app.TranslateXYEnabled = True
                app.TranslateYZEnabled = True
                app.TranslateZXEnabled = True
                'Plane size.
                app.PlanarTranslationGripSize = MyCustomAppearance(8)
                'Plane distance.
                app.PlanarTranslationGripCorner = MyCustomAppearance(9)
            Else
                app.TranslateXYEnabled = False
                app.TranslateYZEnabled = False
                app.TranslateZXEnabled = False
                'Plane size.
                app.PlanarTranslationGripSize = 0
                'Plane distance.
                app.PlanarTranslationGripCorner = 0
            End If

            If (Geometry(i).ObjectType = Rhino.DocObjects.ObjectType.Point) Then
                app.ScaleXEnabled = False
                app.ScaleYEnabled = False
                app.ScaleZEnabled = False
            End If

            Appearances(i) = app

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

    Sub New(Reader As GH_IO.Serialization.GH_IReader, comp As GumballComp)

        If Not (Reader.ChunkExists("gbroot")) Then Exit Sub

        Try
            Dim i As New Integer

            'Root.
            Dim root As GH_IReader = Reader.FindChunk("gbroot")

            'Data.
            Dim data As GH_IReader = root.FindChunk("gbdata", 0)

            'Count.
            Dim countgeo As GH_IReader = data.FindChunk("countgeo", 0)
            Count = countgeo.GetInt32("count", 0)

            'Geomtry.
            Geometry = New GeometryBase(Count - 1) {}
            Dim g As GH_IO.Serialization.GH_IReader = data.FindChunk("geometry", 1)
            For i = 0 To Count - 1
                Dim bytes As Byte() = g.GetByteArray("geo", i)
                Geometry(i) = GH_Convert.ByteArrayToCommonObject(Of GeometryBase)(bytes)
            Next

            'Transform.
            Xform = New Types.GH_Transform(Count - 1) {}
            Dim xf As GH_IO.Serialization.GH_IReader = data.FindChunk("transform", 2)
            For i = 0 To Count - 1
                Dim t As GH_IO.Serialization.GH_IReader = xf.FindChunk("gh_transform", i)
                Dim ghxform As New Types.GH_Transform()
                ghxform.Read(t)
                Xform(i) = ghxform
            Next

            'Gumball.
            Gumballs = New Rhino.UI.Gumball.GumballObject(Count - 1) {}
            Dim go As GH_IO.Serialization.GH_IReader = data.FindChunk("gumball", 3)
            For i = 0 To Count - 1
                Dim gb As New Rhino.UI.Gumball.GumballObject
                Dim frame As New Rhino.UI.Gumball.GumballFrame
                Dim pln As GH_IO.Types.GH_Plane = go.GetPlane("frameplane", i)
                frame.Plane = New Plane(New Point3d(pln.Origin.x, pln.Origin.y, pln.Origin.z), New Vector3d(pln.XAxis.x, pln.XAxis.y, pln.XAxis.z), New Vector3d(pln.YAxis.x, pln.YAxis.y, pln.YAxis.z))
                Dim scd As GH_IO.Types.GH_Point3D = go.GetPoint3D("scalegripdistance", i)
                frame.ScaleGripDistance = New Vector3d(scd.x, scd.y, scd.z)
                gb.Frame = frame
                Gumballs(i) = gb
            Next

            'Attributes.
            Dim att As GH_IReader = root.FindChunk("gbattributes", 1)

            'Attributes_values
            Dim att0 As GH_IO.Serialization.GH_Chunk = att.FindChunk("gumballattributes_values", 0)

            MyCustomAppearance(0) = att0.GetInt32("GbAtt_Translate", 0)
            MyCustomAppearance(1) = att0.GetInt32("GbAtt_PlanarTranslate", 1)
            MyCustomAppearance(2) = att0.GetInt32("GbAtt_FreeTranslate", 2)
            MyCustomAppearance(3) = att0.GetInt32("GbAtt_Rotate", 3)
            MyCustomAppearance(4) = att0.GetInt32("GbAtt_Scale", 4)
            MyCustomAppearance(5) = att0.GetInt32("GbAtt_Radius", 5)
            MyCustomAppearance(6) = att0.GetInt32("GbAtt_ArrowHead", 6)
            MyCustomAppearance(7) = att0.GetInt32("GbAtt_Thickness", 7)
            MyCustomAppearance(8) = att0.GetInt32("GbAtt_PlaneSize", 8)
            MyCustomAppearance(9) = att0.GetInt32("GbAtt_PlaneDistance", 9)


            'Attributes_modes
            Dim att1 As GH_IO.Serialization.GH_Chunk = att.FindChunk("gumballattributes_modes", 1)
            comp.ModeValue(0) = att1.GetInt32("valmode", 0)
            comp.ModeValue(1) = att1.GetInt32("attmode", 1)
            comp.ModeValue(2) = att1.GetBoolean("aligntogeometry", 2)

            'End reader.

            Component = comp
            Conduits = New Rhino.UI.Gumball.GumballDisplayConduit(Count - 1) {}
            Appearances = New Rhino.UI.Gumball.GumballAppearanceSettings(Count - 1) {}
            'MyCallBack = New CustomCallBack(Me)

            For i = 0 To Count - 1

                'Appearance.
                Dim app As New Rhino.UI.Gumball.GumballAppearanceSettings
                app.MenuEnabled = False

                'Translate.
                app.TranslateXEnabled = MyCustomAppearance(0)
                app.TranslateYEnabled = MyCustomAppearance(0)
                app.TranslateZEnabled = MyCustomAppearance(0)
                'Free translate.
                If (MyCustomAppearance(2)) Then
                    app.FreeTranslate = 2
                Else
                    app.FreeTranslate = 0
                End If
                'Rotate.
                app.RotateXEnabled = MyCustomAppearance(3)
                app.RotateYEnabled = MyCustomAppearance(3)
                app.RotateZEnabled = MyCustomAppearance(3)
                'Scale.
                app.ScaleXEnabled = MyCustomAppearance(4)
                app.ScaleYEnabled = MyCustomAppearance(4)
                app.ScaleZEnabled = MyCustomAppearance(4)
                'Radius.
                app.Radius = MyCustomAppearance(5)
                'Head.
                app.ArrowHeadLength = MyCustomAppearance(6) * 2
                app.ArrowHeadWidth = MyCustomAppearance(6)
                'Thickness.
                app.AxisThickness = MyCustomAppearance(7)
                app.ArcThickness = MyCustomAppearance(7)
                'Planar translate.
                If MyCustomAppearance(1) Then
                    app.TranslateXYEnabled = True
                    app.TranslateYZEnabled = True
                    app.TranslateZXEnabled = True
                    'Plane size.
                    app.PlanarTranslationGripSize = MyCustomAppearance(8)
                    'Plane distance.
                    app.PlanarTranslationGripCorner = MyCustomAppearance(9)
                Else
                    app.TranslateXYEnabled = False
                    app.TranslateYZEnabled = False
                    app.TranslateZXEnabled = False
                    'Plane size.
                    app.PlanarTranslationGripSize = 0
                    'Plane distance.
                    app.PlanarTranslationGripCorner = 0
                End If

                If (Geometry(i).ObjectType = Rhino.DocObjects.ObjectType.Point) Then
                    app.ScaleXEnabled = False
                    app.ScaleYEnabled = False
                    app.ScaleZEnabled = False
                End If

                Appearances(i) = app

                'Display conduit.
                Dim conduit As New Rhino.UI.Gumball.GumballDisplayConduit
                conduit.SetBaseGumball(Gumballs(i), app)
                Conduits(i) = conduit
            Next

        Catch ex As Exception
            Rhino.RhinoApp.WriteLine("NEW_GB; " & ex.ToString())
        End Try
    End Sub
    '
    '
    Public Sub ShowGumballs()
        For i As Int32 = 0 To Count - 1
            Conduits(i).Enabled = True
        Next
        Me.Enabled = True
        Rhino.RhinoDoc.ActiveDoc.Views.Redraw()
    End Sub

    Public Sub HideGumballs()
        For i As Int32 = 0 To Count - 1
            Conduits(i).Enabled = False
        Next
        Me.Enabled = False
        Rhino.RhinoDoc.ActiveDoc.Views.Redraw()
    End Sub

    Public Sub Dispose()
        For i As Int32 = 0 To Count - 1
            Conduits(i).Enabled = False
            Conduits(i).Dispose()
            Gumballs(i).Dispose()
        Next
        Me.Enabled = False
    End Sub
#End Region

#Region "Gumball Transform"

    Public Sub UpdateGumball(ByVal Index As Integer)

        If Not (Conduits(Index).InRelocate) Then
            Dim xform As Transform = Conduits(Index).TotalTransform
            Conduits(Index).PreTransform = xform
        End If
        Dim gbframe As Rhino.UI.Gumball.GumballFrame = Conduits(Index).Gumball.Frame
        Dim baseFrame As Rhino.UI.Gumball.GumballFrame = Gumballs(Index).Frame

        If (Rhino.ApplicationSettings.ModelAidSettings.GridSnap) Then
            If (Conduits(Index).PickResult.Mode = Rhino.UI.Gumball.GumballMode.TranslateFree Or Conduits(Index).PickResult.Mode = Rhino.UI.Gumball.GumballMode.TranslateX Or
                    Conduits(Index).PickResult.Mode = Rhino.UI.Gumball.GumballMode.TranslateY Or Conduits(Index).PickResult.Mode = Rhino.UI.Gumball.GumballMode.TranslateZ Or
                    Conduits(Index).PickResult.Mode = Rhino.UI.Gumball.GumballMode.TranslateXY Or Conduits(Index).PickResult.Mode = Rhino.UI.Gumball.GumballMode.TranslateYZ Or
                    Conduits(Index).PickResult.Mode = Rhino.UI.Gumball.GumballMode.TranslateZX) Then
                Dim pln As Plane = gbframe.Plane
                pln.Origin = New Point3d(CInt(pln.Origin.X), CInt(pln.Origin.Y), CInt(pln.Origin.Z))
                gbframe.Plane = pln
            End If
        End If
        baseFrame.Plane = gbframe.Plane
        baseFrame.ScaleGripDistance = gbframe.ScaleGripDistance
        Gumballs(Index).Frame = baseFrame
        Conduits(Index).SetBaseGumball(Gumballs(Index), Appearances(Index))
        Conduits(Index).Enabled = True

        If (Me.Component.ModeValue(2)) AndAlso (GeometrytoAlign IsNot Nothing) Then AlignToGeometry(GeometrytoAlign)

        Rhino.RhinoDoc.ActiveDoc.Views.Redraw()

    End Sub

    Public Sub UpdateGumball(ByVal i As Integer, xform As Transform)

        Dim gbframe As Rhino.UI.Gumball.GumballFrame = Conduits(i).Gumball.Frame
        Dim baseFrame As Rhino.UI.Gumball.GumballFrame = Gumballs(i).Frame
        Dim pln As Plane = gbframe.Plane
        pln.Transform(xform)

        If (Rhino.ApplicationSettings.ModelAidSettings.GridSnap) Then
            If (Conduits(Index).PickResult.Mode = Rhino.UI.Gumball.GumballMode.TranslateFree Or Conduits(Index).PickResult.Mode = Rhino.UI.Gumball.GumballMode.TranslateX Or
                    Conduits(Index).PickResult.Mode = Rhino.UI.Gumball.GumballMode.TranslateY Or Conduits(Index).PickResult.Mode = Rhino.UI.Gumball.GumballMode.TranslateZ Or
                    Conduits(Index).PickResult.Mode = Rhino.UI.Gumball.GumballMode.TranslateXY Or Conduits(Index).PickResult.Mode = Rhino.UI.Gumball.GumballMode.TranslateYZ Or
                    Conduits(Index).PickResult.Mode = Rhino.UI.Gumball.GumballMode.TranslateZX) Then

                pln.Origin = New Point3d(CInt(pln.Origin.X), CInt(pln.Origin.Y), CInt(pln.Origin.Z))
                gbframe.Plane = pln
            End If
        End If

        baseFrame.Plane = pln
        baseFrame.ScaleGripDistance = gbframe.ScaleGripDistance
        Gumballs(i).Frame = baseFrame
        Conduits(i).SetBaseGumball(Gumballs(i), Appearances(i))
        Conduits(i).Enabled = True

        If (Me.Component.ModeValue(2)) AndAlso (GeometrytoAlign IsNot Nothing) Then AlignToGeometry(GeometrytoAlign)


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

    Public Sub UpdateGumballFromTextBox(ByVal index As Integer, ByVal Transform As Transform)

        Conduits(index).PreTransform = Transform

        Dim gbframe As Rhino.UI.Gumball.GumballFrame = Conduits(index).Gumball.Frame
        Dim baseFrame As Rhino.UI.Gumball.GumballFrame = Gumballs(index).Frame

        Dim pln As Plane = gbframe.Plane
        pln.Transform(Transform)
        gbframe.Plane = pln
        baseFrame.Plane = gbframe.Plane

        baseFrame.ScaleGripDistance = gbframe.ScaleGripDistance

        Gumballs(index).Frame = baseFrame
        Conduits(index).SetBaseGumball(Gumballs(index), Appearances(index))
        Conduits(index).Enabled = True

        If (Me.Component.ModeValue(2)) AndAlso (GeometrytoAlign IsNot Nothing) Then AlignToGeometry(GeometrytoAlign)

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
            baseFrame.ScaleGripDistance = gbframe.ScaleGripDistance
            Gumballs(i).Frame = baseFrame
            Conduits(i).SetBaseGumball(Gumballs(i), Appearances(i))
            Conduits(i).Enabled = True

        Next
        Rhino.RhinoDoc.ActiveDoc.Views.Redraw()
    End Sub
#End Region

#Region "MouseCallback"
    Private Index As Integer = -1
    Private SaveUndo As Boolean = False
    Private TextBox As FormTextBox = Nothing
    Public ValueString As String = String.Empty
    Private Keyboard As New Microsoft.VisualBasic.Devices.Keyboard

    Protected Overrides Sub OnMouseDown(e As Rhino.UI.MouseCallbackEventArgs)
        MyBase.OnMouseDown(e)
        Index = -1

        If (TextBox IsNot Nothing) Then
            TextBox.Dispose()
            TextBox.Close()
            TextBox = Nothing
        End If

        If (e.Button <> MouseButtons.Left) Then Exit Sub

        Dim Pick As New Rhino.Input.Custom.PickContext
        Pick.View = e.View
        Pick.PickStyle = Rhino.Input.Custom.PickStyle.PointPick
        Pick.SetPickTransform(e.View.ActiveViewport.GetPickTransform(e.ViewportPoint))
        Dim pickline As Line = Nothing
        e.View.ActiveViewport.GetFrustumLine(CDbl(e.ViewportPoint.X), CDbl(e.ViewportPoint.Y), pickline)
        Pick.PickLine = pickline
        Pick.UpdateClippingPlanes()

        For i As Int32 = 0 To Count - 1
            If (Conduits(i).PickGumball(Pick, Nothing)) Then
                Index = i
                SaveUndo = True
                e.Cancel = True
                If (Keyboard.CtrlKeyDown) Then
                    TextBox = New FormTextBox(Rhino.RhinoDoc.ActiveDoc.Views.ActiveView.ClientToScreen(e.ViewportPoint), Me)
                End If
                Exit For
            End If
        Next

    End Sub

    Protected Overrides Sub OnMouseMove(e As Rhino.UI.MouseCallbackEventArgs)
        MyBase.OnMouseMove(e)

        If (TextBox IsNot Nothing) Then
            e.Cancel = True
            Exit Sub
        End If

        If (Index = -1) Or (Index >= Count) Then Exit Sub

        If (Conduits(Index).PickResult.Mode = Rhino.UI.Gumball.GumballMode.None) Then Exit Sub
        'Conduits(Index).CheckShiftAndControlKeys()
        Dim wordline As Line = Nothing
        If Not (e.View.MainViewport.GetFrustumLine(CDbl(e.ViewportPoint.X), CDbl(e.ViewportPoint.Y), wordline)) Then
            wordline = Line.Unset
        End If
        Dim cplane As Plane = e.View.MainViewport.GetConstructionPlane().Plane()
        Dim lp As Double = Nothing
        Rhino.Geometry.Intersect.Intersection.LinePlane(wordline, cplane, lp)
        Dim dragPoint As Point3d = wordline.PointAt(lp)
        If (Rhino.ApplicationSettings.ModelAidSettings.GridSnap) Then
            If (Conduits(Index).PickResult.Mode = Rhino.UI.Gumball.GumballMode.TranslateFree Or Conduits(Index).PickResult.Mode = Rhino.UI.Gumball.GumballMode.TranslateX Or
                    Conduits(Index).PickResult.Mode = Rhino.UI.Gumball.GumballMode.TranslateY Or Conduits(Index).PickResult.Mode = Rhino.UI.Gumball.GumballMode.TranslateZ Or
                    Conduits(Index).PickResult.Mode = Rhino.UI.Gumball.GumballMode.TranslateXY Or Conduits(Index).PickResult.Mode = Rhino.UI.Gumball.GumballMode.TranslateYZ Or
                    Conduits(Index).PickResult.Mode = Rhino.UI.Gumball.GumballMode.TranslateZX) Then
                Dim snap As Point3d = New Point3d(CInt(dragPoint.X), CInt(dragPoint.Y), CInt(dragPoint.Z))
                wordline.Transform(Transform.Translation(New Vector3d(snap - dragPoint)))
                dragPoint = snap
            End If
        End If
        If Not (Conduits(Index).UpdateGumball(dragPoint, wordline)) Then Exit Sub
        Rhino.RhinoDoc.ActiveDoc.Views.Redraw()
        e.Cancel = True
    End Sub

    Protected Overrides Sub OnMouseUp(e As Rhino.UI.MouseCallbackEventArgs)
        MyBase.OnMouseUp(e)

        If (Index = -1) Or (Index >= Count) Or (ValueString <> String.Empty) Then Exit Sub

        If (SaveUndo) Then
            Component.RecordUndoEvent("Gumball Drag", New GbUndo(Me))
            SaveUndo = False
        End If

        Dim gbxform As Transform = Conduits(Index).GumballTransform

        If (gbxform <> Transform.Identity) Then

            Select Case Component.ModeValue(0)

                Case 0 'Normal
                    Dim ghXform As New Grasshopper.Kernel.Types.GH_Transform(New Grasshopper.Kernel.Types.Transforms.Generic(gbxform))
                    For Each t As Grasshopper.Kernel.Types.Transforms.ITransform In ghXform.CompoundTransforms
                        Xform(Index).CompoundTransforms.Add(t.Duplicate())
                    Next
                    Xform(Index).ClearCaches()
                    Geometry(Index).Transform(gbxform)
                    UpdateGumball(Index)

                Case 1 'Apply to all.
                    Dim ghXform As New Grasshopper.Kernel.Types.GH_Transform(New Grasshopper.Kernel.Types.Transforms.Generic(gbxform))
                    For i As Int32 = 0 To Count - 1
                        For Each t As Grasshopper.Kernel.Types.Transforms.ITransform In ghXform.CompoundTransforms
                            Xform(i).CompoundTransforms.Add(t.Duplicate())
                        Next
                        Xform(i).ClearCaches()
                        Geometry(i).Transform(gbxform)
                        If (i = Index) Then
                            UpdateGumball(i)
                        Else
                            UpdateGumball(i, gbxform)
                        End If
                    Next
                    Rhino.RhinoDoc.ActiveDoc.Views.Redraw()

                Case 2 'Relocate.
                    UpdateGumball(Index)

            End Select

            Component.ExpireSolution(True)
        End If
        Index = -1
        e.Cancel = True
    End Sub

    Public Sub TransformFromTextBox()

        If (TextBox IsNot Nothing) AndAlso (ValueString IsNot String.Empty) AndAlso (Index > -1) Then

            Me.Component.RecordUndoEvent("Gumball Drag", New GbUndo(Me))

            Dim gbxform As Transform = Conduits(Index).GumballTransform

            Dim value As New Double
            Try
                value = Convert.ToDouble(ValueString)

            Catch ex As Exception
                Rhino.RhinoApp.WriteLine("Invalid value. Only numerical values are allowed.")
                Index = -1
                ValueString = String.Empty
                Exit Sub
            End Try

            Dim pln As Plane = Conduits(Index).Gumball.Frame.Plane

            Select Case Conduits(Index).PickResult.Mode
                Case Rhino.UI.Gumball.GumballMode.TranslateX
                    gbxform = Transform.Translation(pln.XAxis * value)

                Case Rhino.UI.Gumball.GumballMode.TranslateY
                    gbxform = Transform.Translation(pln.YAxis * value)

                Case Rhino.UI.Gumball.GumballMode.TranslateZ
                    gbxform = Transform.Translation(pln.ZAxis * value)

                Case Rhino.UI.Gumball.GumballMode.RotateX
                    gbxform = Transform.Rotation((value * Math.PI) / 180, pln.XAxis, pln.Origin)

                Case Rhino.UI.Gumball.GumballMode.RotateY
                    gbxform = Transform.Rotation((value * Math.PI) / 180, pln.YAxis, pln.Origin)

                Case Rhino.UI.Gumball.GumballMode.RotateZ
                    gbxform = Transform.Rotation((value * Math.PI) / 180, pln.ZAxis, pln.Origin)

                Case Rhino.UI.Gumball.GumballMode.ScaleX
                    gbxform = Transform.Scale(pln, value, 1, 1)
                    If (Component.ModeValue(0) = 1) Then 'Apply to all.
                        For i As Int32 = 0 To Count - 1
                            Dim frame As Rhino.UI.Gumball.GumballFrame = Conduits(i).Gumball.Frame
                            frame.ScaleGripDistance = New Vector3d(frame.ScaleGripDistance.X * value, frame.ScaleGripDistance.Y, frame.ScaleGripDistance.Z)
                            Gumballs(i).Frame = frame
                            Conduits(i).SetBaseGumball(Gumballs(i), Appearances(i))
                        Next
                    Else
                        Dim frame As Rhino.UI.Gumball.GumballFrame = Conduits(Index).Gumball.Frame
                        frame.ScaleGripDistance = New Vector3d(frame.ScaleGripDistance.X * value, frame.ScaleGripDistance.Y, frame.ScaleGripDistance.Z)
                        Gumballs(Index).Frame = frame
                        Conduits(Index).SetBaseGumball(Gumballs(Index), Appearances(Index))
                    End If

                Case Rhino.UI.Gumball.GumballMode.ScaleY
                    gbxform = Transform.Scale(pln, 1, value, 1)
                    If (Component.ModeValue(0) = 1) Then 'Apply to all.
                        For i As Int32 = 0 To Count - 1
                            Dim frame As Rhino.UI.Gumball.GumballFrame = Conduits(i).Gumball.Frame
                            frame.ScaleGripDistance = New Vector3d(frame.ScaleGripDistance.X, frame.ScaleGripDistance.Y * value, frame.ScaleGripDistance.Z)
                            Gumballs(i).Frame = frame
                            Conduits(i).SetBaseGumball(Gumballs(i), Appearances(i))
                        Next
                    Else
                        Dim frame As Rhino.UI.Gumball.GumballFrame = Conduits(Index).Gumball.Frame
                        frame.ScaleGripDistance = New Vector3d(frame.ScaleGripDistance.X, frame.ScaleGripDistance.Y * value, frame.ScaleGripDistance.Z)
                        Gumballs(Index).Frame = frame
                        Conduits(Index).SetBaseGumball(Gumballs(Index), Appearances(Index))
                    End If

                Case Rhino.UI.Gumball.GumballMode.ScaleZ
                    gbxform = Transform.Scale(pln, 1, 1, value)
                    If (Component.ModeValue(0) = 1) Then 'Apply to all.
                        For i As Int32 = 0 To Count - 1
                            Dim frame As Rhino.UI.Gumball.GumballFrame = Conduits(i).Gumball.Frame
                            frame.ScaleGripDistance = New Vector3d(frame.ScaleGripDistance.X, frame.ScaleGripDistance.Y, frame.ScaleGripDistance.Z * value)
                            Gumballs(i).Frame = frame
                            Conduits(i).SetBaseGumball(Gumballs(i), Appearances(i))
                        Next
                    Else
                        Dim frame As Rhino.UI.Gumball.GumballFrame = Conduits(Index).Gumball.Frame
                        frame.ScaleGripDistance = New Vector3d(frame.ScaleGripDistance.X, frame.ScaleGripDistance.Y, frame.ScaleGripDistance.Z * value)
                        Gumballs(Index).Frame = frame
                        Conduits(Index).SetBaseGumball(Gumballs(Index), Appearances(Index))
                    End If

                Case Else
                    Index = -1
                    ValueString = String.Empty
                    Exit Sub

            End Select

            If (gbxform <> Transform.Identity) Then

                Select Case Component.ModeValue(0)

                    Case 0 'Normal
                        Dim ghXform As New Grasshopper.Kernel.Types.GH_Transform(New Grasshopper.Kernel.Types.Transforms.Generic(gbxform))
                        For Each t As Grasshopper.Kernel.Types.Transforms.ITransform In ghXform.CompoundTransforms
                            Xform(Index).CompoundTransforms.Add(t.Duplicate())
                        Next
                        Xform(Index).ClearCaches()
                        Geometry(Index).Transform(gbxform)
                        UpdateGumballFromTextBox(Index, gbxform)

                    Case 1 'Apply to all.
                        Dim ghXform As New Grasshopper.Kernel.Types.GH_Transform(New Grasshopper.Kernel.Types.Transforms.Generic(gbxform))
                        For i As Int32 = 0 To Count - 1
                            For Each t As Grasshopper.Kernel.Types.Transforms.ITransform In ghXform.CompoundTransforms
                                Xform(i).CompoundTransforms.Add(t.Duplicate())
                            Next
                            Xform(i).ClearCaches()
                            Geometry(i).Transform(gbxform)
                            UpdateGumballFromTextBox(i, gbxform)
                        Next

                    Case 2 'Relocate.
                        UpdateGumballFromTextBox(Index, gbxform)

                End Select
                Component.ExpireSolution(True)
            End If
            Index = -1
            ValueString = String.Empty
        End If
    End Sub
#End Region

#Region "Appearance"
    Public Property CustomAppearance(ByVal index As Integer) As Integer
        Get
            Return MyCustomAppearance(index)
        End Get
        Set(value As Integer)
            MyCustomAppearance(index) = value
            Select Case index
                Case 0
                    For i As Integer = 0 To Count - 1
                        Appearances(i).TranslateXEnabled = value
                        Appearances(i).TranslateYEnabled = value
                        Appearances(i).TranslateZEnabled = value
                    Next
                Case 1
                    For i As Integer = 0 To Count - 1
                        Appearances(i).TranslateXYEnabled = value
                        Appearances(i).TranslateYZEnabled = value
                        Appearances(i).TranslateZXEnabled = value
                    Next
                Case 2
                    For i As Integer = 0 To Count - 1
                        Appearances(i).FreeTranslate = If(value, 2, 0)
                    Next
                Case 3
                    For i As Integer = 0 To Count - 1
                        Appearances(i).RotateXEnabled = value
                        Appearances(i).RotateYEnabled = value
                        Appearances(i).RotateZEnabled = value
                    Next
                Case 4
                    For i As Integer = 0 To Count - 1
                        Appearances(i).ScaleXEnabled = value
                        Appearances(i).ScaleYEnabled = value
                        Appearances(i).ScaleZEnabled = value
                    Next
                Case 5
                    For i As Integer = 0 To Count - 1
                        Appearances(i).Radius = value
                    Next
                Case 6
                    For i As Integer = 0 To Count - 1
                        Appearances(i).ArrowHeadLength = value * 2
                        Appearances(i).ArrowHeadWidth = value
                    Next
                Case 7
                    For i As Integer = 0 To Count - 1
                        Appearances(i).ArcThickness = value
                        Appearances(i).AxisThickness = value
                    Next
                Case 8
                    For i As Integer = 0 To Count - 1
                        Appearances(i).PlanarTranslationGripSize = If(MyCustomAppearance(1), value, 0)
                    Next
                Case 9
                    For i As Integer = 0 To Count - 1
                        Appearances(i).PlanarTranslationGripCorner = If(MyCustomAppearance(1), value, 0)
                    Next
                Case Else
                    Throw New ArgumentOutOfRangeException()
            End Select
            ChangeAppearances()
        End Set
    End Property

    Public Property CustomAppearance As Integer()
        Get
            Return MyCustomAppearance
        End Get
        Set(value As Integer())
            MyCustomAppearance = value

            For i As Int32 = 0 To Count - 1
                'Appearance.
                Dim app As Rhino.UI.Gumball.GumballAppearanceSettings = Appearances(i)
                app.MenuEnabled = False

                'Translate.
                app.TranslateXEnabled = MyCustomAppearance(0)
                app.TranslateYEnabled = MyCustomAppearance(0)
                app.TranslateZEnabled = MyCustomAppearance(0)
                'Free translate.
                If (MyCustomAppearance(2)) Then
                    app.FreeTranslate = 2
                Else
                    app.FreeTranslate = 0
                End If
                'Rotate.
                app.RotateXEnabled = MyCustomAppearance(3)
                app.RotateYEnabled = MyCustomAppearance(3)
                app.RotateZEnabled = MyCustomAppearance(3)
                'Scale.
                app.ScaleXEnabled = MyCustomAppearance(4)
                app.ScaleYEnabled = MyCustomAppearance(4)
                app.ScaleZEnabled = MyCustomAppearance(4)
                'Radius.
                app.Radius = MyCustomAppearance(5)
                'Head.
                app.ArrowHeadLength = MyCustomAppearance(6) * 2
                app.ArrowHeadWidth = MyCustomAppearance(6)
                'Thickness.
                app.AxisThickness = MyCustomAppearance(7)
                app.ArcThickness = MyCustomAppearance(7)
                'Planar translate.
                If MyCustomAppearance(1) Then
                    app.TranslateXYEnabled = True
                    app.TranslateYZEnabled = True
                    app.TranslateZXEnabled = True
                    'Plane size.
                    app.PlanarTranslationGripSize = MyCustomAppearance(8)
                    'Plane distance.
                    app.PlanarTranslationGripCorner = MyCustomAppearance(9)
                Else
                    app.TranslateXYEnabled = False
                    app.TranslateYZEnabled = False
                    app.TranslateZXEnabled = False
                    'Plane size.
                    app.PlanarTranslationGripSize = 0
                    'Plane distance.
                    app.PlanarTranslationGripCorner = 0
                End If

                If (Geometry(i).ObjectType = Rhino.DocObjects.ObjectType.Point) Then
                    app.ScaleXEnabled = False
                    app.ScaleYEnabled = False
                    app.ScaleZEnabled = False
                End If

                Appearances(i) = app
            Next

            ChangeAppearances()
        End Set
    End Property

    Private Sub ChangeAppearances()

        For i As Int32 = 0 To Count - 1
            Conduits(i).Enabled = False
            Conduits(i).SetBaseGumball(Gumballs(i), Appearances(i))
            Conduits(i).Enabled = True
        Next
        Rhino.RhinoDoc.ActiveDoc.Views.Redraw()
    End Sub
#End Region

#Region "Serializable"

    Public Function GumballWriter(ByVal writer As GH_IO.Serialization.GH_IWriter) As Boolean
        'gbroot 
        '   |
        '   ├─gbdata
        '   |     |
        '   |     ├─countgeo
        '   |     |       |
        '   |     |       └─count
        '   |     |
        '   |     ├─geometry
        '   |     |       |
        '   |     |       ├─geo
        '   |     |      (i)           
        '   |     | 
        '   |     ├─transform
        '   |     |       |
        '   |     |       ├─gh_transform
        '   |     |      (i)
        '   |     |  
        '   |     └─gumball
        '   |             |
        '   |             ├─frameplane
        '   |             ├─scalegripdistance
        '   |            (i)
        '   | 
        '   └─gbattributes 
        '         |
        '         ├─gumballattributes_values
        '         |                     |
        '         |                     ├─GbAtt_Translate
        '         |                     ├─GbAtt_PlanarTranslate
        '         |                     ├─GbAtt_FreeTranslate
        '         |                     ├─GbAtt_Rotate
        '         |                     ├─GbAtt_Scale
        '         |                     ├─GbAtt_Radius
        '         |                     ├─GbAtt_ArrowHead
        '         |                     ├─GbAtt_Thickness
        '         |                     ├─GbAtt_PlaneSize
        '         |                     └─GbAtt_PlaneDistance
        '         | 
        '         └─gumballattributes_modes
        '                               |
        '                               ├─valmode
        '                               ├─attmode
        '                               └─aligntogeometry

        Try
            Dim i As New Integer

            'Root.
            ' writer.RemoveChunk("gbroot")
            Dim root As GH_IWriter = writer.CreateChunk("gbroot")

            'Data.
            Dim data As GH_IWriter = root.CreateChunk("gbdata", 0)

            'Count.
            Dim co As GH_IWriter = data.CreateChunk("countgeo", 0)
            co.SetInt32("count", 0, Me.Count)

            'Geometry.
            Dim geo As GH_IWriter = data.CreateChunk("geometry", 1)
            For i = 0 To Count - 1
                Dim bytes As Byte() = GH_Convert.CommonObjectToByteArray(Geometry(i))
                geo.SetByteArray("geo", i, bytes)
            Next

            'Transform.
            Dim xf As GH_IWriter = data.CreateChunk("transform", 2)
            For i = 0 To Count - 1
                Dim t As GH_IWriter = xf.CreateChunk("gh_transform", i)
                Xform(i).Write(t)
            Next

            'Gumball.
            Dim obj As GH_IWriter = data.CreateChunk("gumball", 3)
            For i = 0 To Count - 1
                Dim frame As Plane = Gumballs(i).Frame.Plane
                Dim pln As GH_IO.Types.GH_Plane
                pln.Origin = New GH_IO.Types.GH_Point3D(frame.Origin.X, frame.Origin.Y, frame.Origin.Z)
                pln.XAxis = New GH_IO.Types.GH_Point3D(frame.XAxis.X, frame.XAxis.Y, frame.XAxis.Z)
                pln.YAxis = New GH_IO.Types.GH_Point3D(frame.YAxis.X, frame.YAxis.Y, frame.YAxis.Z)
                obj.SetPlane("frameplane", i, pln)
                Dim scd As Vector3d = Gumballs(i).Frame.ScaleGripDistance
                Dim vec As New GH_IO.Types.GH_Point3D(scd.X, scd.Y, scd.Z)
                obj.SetPoint3D("scalegripdistance", i, vec)
            Next

            'Attributes.
            Dim att As GH_IWriter = root.CreateChunk("gbattributes", 1)

            'Attributes_values
            Dim att0 As GH_IWriter = att.CreateChunk("gumballattributes_values", 0)
            att0.SetInt32("GbAtt_Translate", 0, MyCustomAppearance(0))
            att0.SetInt32("GbAtt_PlanarTranslate", 1, MyCustomAppearance(1))
            att0.SetInt32("GbAtt_FreeTranslate", 2, MyCustomAppearance(2))
            att0.SetInt32("GbAtt_Rotate", 3, MyCustomAppearance(3))
            att0.SetInt32("GbAtt_Scale", 4, MyCustomAppearance(4))
            att0.SetInt32("GbAtt_Radius", 5, MyCustomAppearance(5))
            att0.SetInt32("GbAtt_ArrowHead", 6, MyCustomAppearance(6))
            att0.SetInt32("GbAtt_Thickness", 7, MyCustomAppearance(7))
            att0.SetInt32("GbAtt_PlaneSize", 8, MyCustomAppearance(8))
            att0.SetInt32("GbAtt_PlaneDistance", 9, MyCustomAppearance(9))

            'Attributes_modes
            Dim att1 As GH_IWriter = att.CreateChunk("gumballattributes_modes", 1)
            att1.SetInt32("valmode", 0, Me.Component.ModeValue(0))
            att1.SetInt32("attmode", 1, Me.Component.ModeValue(1))
            att1.SetBoolean("aligntogeometry", 2, Me.Component.ModeValue(2))

        Catch ex As Exception
            Rhino.RhinoApp.WriteLine("WRITER_GB; " & ex.ToString())
        End Try
        Return True
    End Function

    Public Function GumballReader(Reader As GH_IO.Serialization.GH_IReader) As Boolean

        If Not (Reader.ChunkExists("gbroot")) Then
            Return False
            Exit Function
        End If

        Try
            Dim i As New Integer

            'Root.
            Dim root As GH_IReader = Reader.FindChunk("gbroot")

            'Data.
            Dim data As GH_IReader = root.FindChunk("gbdata", 0)

            'Count.
            Dim countgeo As GH_IReader = data.FindChunk("countgeo", 0)
            Count = countgeo.GetInt32("count", 0)

            'Geomtry.
            Geometry = New GeometryBase(Count - 1) {}
            Dim g As GH_IO.Serialization.GH_IReader = data.FindChunk("geometry", 1)
            For i = 0 To Count - 1
                Dim bytes As Byte() = g.GetByteArray("geo", i)
                Geometry(i) = GH_Convert.ByteArrayToCommonObject(Of GeometryBase)(bytes)
            Next

            'Transform.
            Xform = New Types.GH_Transform(Count - 1) {}
            Dim xf As GH_IO.Serialization.GH_IReader = data.FindChunk("transform", 2)
            For i = 0 To Count - 1
                Dim t As GH_IO.Serialization.GH_IReader = xf.FindChunk("gh_transform", i)
                Dim ghxform As New Types.GH_Transform()
                ghxform.Read(t)
                Xform(i) = ghxform
            Next

            'Gumball.
            Gumballs = New Rhino.UI.Gumball.GumballObject(Count - 1) {}
            Dim go As GH_IO.Serialization.GH_IReader = data.FindChunk("gumball", 3)
            For i = 0 To Count - 1
                Dim gb As New Rhino.UI.Gumball.GumballObject
                Dim frame As New Rhino.UI.Gumball.GumballFrame
                Dim pln As GH_IO.Types.GH_Plane = go.GetPlane("frameplane", i)
                frame.Plane = New Plane(New Point3d(pln.Origin.x, pln.Origin.y, pln.Origin.z), New Vector3d(pln.XAxis.x, pln.XAxis.y, pln.XAxis.z), New Vector3d(pln.YAxis.x, pln.YAxis.y, pln.YAxis.z))
                Dim scd As GH_IO.Types.GH_Point3D = go.GetPoint3D("scalegripdistance", i)
                frame.ScaleGripDistance = New Vector3d(scd.x, scd.y, scd.z)
                gb.Frame = frame
                Gumballs(i) = gb
            Next

            'Attributes.
            Dim att As GH_IReader = root.FindChunk("gbattributes", 1)

            'Attributes_values
            Dim att0 As GH_IO.Serialization.GH_Chunk = att.FindChunk("gumballattributes_values", 0)

            MyCustomAppearance(0) = att0.GetInt32("GbAtt_Translate", 0)
            MyCustomAppearance(1) = att0.GetInt32("GbAtt_PlanarTranslate", 1)
            MyCustomAppearance(2) = att0.GetInt32("GbAtt_FreeTranslate", 2)
            MyCustomAppearance(3) = att0.GetInt32("GbAtt_Rotate", 3)
            MyCustomAppearance(4) = att0.GetInt32("GbAtt_Scale", 4)
            MyCustomAppearance(5) = att0.GetInt32("GbAtt_Radius", 5)
            MyCustomAppearance(6) = att0.GetInt32("GbAtt_ArrowHead", 6)
            MyCustomAppearance(7) = att0.GetInt32("GbAtt_Thickness", 7)
            MyCustomAppearance(8) = att0.GetInt32("GbAtt_PlaneSize", 8)
            MyCustomAppearance(9) = att0.GetInt32("GbAtt_PlaneDistance", 9)

            'Attributes_modes
            Dim att1 As GH_IO.Serialization.GH_Chunk = att.FindChunk("gumballattributes_modes", 1)
            Component.ModeValue(0) = att1.GetInt32("valmode", 0)
            Component.ModeValue(1) = att1.GetInt32("attmode", 1)
            Component.ModeValue(2) = att1.GetBoolean("aligntogeometry", 2)

            'End reader.

            For i = 0 To Count - 1
                'Appearance.
                Dim app As Rhino.UI.Gumball.GumballAppearanceSettings = Appearances(i)
                app.MenuEnabled = False

                'Translate.
                app.TranslateXEnabled = MyCustomAppearance(0)
                app.TranslateYEnabled = MyCustomAppearance(0)
                app.TranslateZEnabled = MyCustomAppearance(0)
                'Free translate.
                If (MyCustomAppearance(2)) Then
                    app.FreeTranslate = 2
                Else
                    app.FreeTranslate = 0
                End If
                'Rotate.
                app.RotateXEnabled = MyCustomAppearance(3)
                app.RotateYEnabled = MyCustomAppearance(3)
                app.RotateZEnabled = MyCustomAppearance(3)
                'Scale.
                app.ScaleXEnabled = MyCustomAppearance(4)
                app.ScaleYEnabled = MyCustomAppearance(4)
                app.ScaleZEnabled = MyCustomAppearance(4)
                'Radius.
                app.Radius = MyCustomAppearance(5)
                'Head.
                app.ArrowHeadLength = MyCustomAppearance(6) * 2
                app.ArrowHeadWidth = MyCustomAppearance(6)
                'Thickness.
                app.AxisThickness = MyCustomAppearance(7)
                app.ArcThickness = MyCustomAppearance(7)
                'Planar translate.
                If MyCustomAppearance(1) Then
                    app.TranslateXYEnabled = True
                    app.TranslateYZEnabled = True
                    app.TranslateZXEnabled = True
                    'Plane size.
                    app.PlanarTranslationGripSize = MyCustomAppearance(8)
                    'Plane distance.
                    app.PlanarTranslationGripCorner = MyCustomAppearance(9)
                Else
                    app.TranslateXYEnabled = False
                    app.TranslateYZEnabled = False
                    app.TranslateZXEnabled = False
                    'Plane size.
                    app.PlanarTranslationGripSize = 0
                    'Plane distance.
                    app.PlanarTranslationGripCorner = 0
                End If

                If (Geometry(i).ObjectType = Rhino.DocObjects.ObjectType.Point) Then
                    app.ScaleXEnabled = False
                    app.ScaleYEnabled = False
                    app.ScaleZEnabled = False
                End If

                Appearances(i) = app

                'Display conduit.
                Conduits(i).SetBaseGumball(Gumballs(i), app)

                Me.Component.ExpireSolution(True)
                If (Me.Component.Attributes.Selected) Then Me.ShowGumballs()

            Next
        Catch ex As Exception
            Rhino.RhinoApp.WriteLine("READER_GB; " & ex.ToString())

        End Try
        Return True
    End Function
    '
    '
    Public Function Write(writer As GH_IWriter) As Boolean Implements GH_ISerializable.Write
        GumballWriter(writer)
        Return True
    End Function

    Public Function Read(reader As GH_IReader) As Boolean Implements GH_ISerializable.Read
        GumballReader(reader)
        Return True
    End Function
#End Region

End Class

Public Class GbUndo
    Inherits Grasshopper.Kernel.Undo.GH_ArchivedUndoAction

    Private GB As GhGumball

    Sub New(MyGb As GhGumball)
        GB = MyGb
        Me.m_data = Me.SerializeToByteArray(MyGb)
        Dim chunk As New GH_LooseChunk("GbUndo")
        Me.Write(chunk)
    End Sub

    Protected Overrides Sub Internal_Redo(doc As GH_Document)
        Internal_Undo(doc)
    End Sub

    Protected Overrides Sub Internal_Undo(doc As GH_Document)
        Dim reader As New GH_LooseChunk("GbUndo")
        reader.Deserialize_Binary(Me.m_data)
        GB.Read(reader)
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
        If (Component.MyGumball IsNot Nothing) Then
            Me.ButtTranslate.Checked = Component.MyGumball.CustomAppearance(0)
            Me.ButtPlane.Checked = Component.MyGumball.CustomAppearance(1)
            Me.ButtFree.Checked = Component.MyGumball.CustomAppearance(2)
            Me.ButtRotate.Checked = Component.MyGumball.CustomAppearance(3)
            Me.ButtScale.Checked = Component.MyGumball.CustomAppearance(4)
            Me.NumRad.Value = New Decimal(New Integer() {Component.MyGumball.CustomAppearance(5), 0, 0, 0})
            Me.NumAH.Value = New Decimal(New Integer() {Component.MyGumball.CustomAppearance(6), 0, 0, 0})
            Me.NumThk.Value = New Decimal(New Integer() {Component.MyGumball.CustomAppearance(7), 0, 0, 0})
            Me.NumPS.Value = New Decimal(New Integer() {Component.MyGumball.CustomAppearance(8), 0, 0, 0})
            Me.NumPD.Value = New Decimal(New Integer() {Component.MyGumball.CustomAppearance(9), 0, 0, 0})
        End If
        CanSend = True
    End Sub

    Private Sub Form1_FormClosed(sender As Object, e As FormClosedEventArgs) Handles MyBase.FormClosed
        If (Component.MyGumball IsNot Nothing) Then Component.RecordUndoEvent("Gumball Attributes", New GbUndo(Component.MyGumball))
        Me.Component.AttForm = Nothing
    End Sub

    Private Sub ButtTranslate_CheckedChanged(sender As Object, e As EventArgs) Handles ButtTranslate.CheckedChanged
        If (Me.Component.MyGumball IsNot Nothing) AndAlso (CanSend) Then Component.MyGumball.CustomAppearance(0) = Me.ButtTranslate.CheckState.value__
    End Sub

    Private Sub ButtPlane_CheckedChanged(sender As Object, e As EventArgs) Handles ButtPlane.CheckedChanged
        If (Me.Component.MyGumball IsNot Nothing) AndAlso (CanSend) Then Component.MyGumball.CustomAppearance(1) = Me.ButtPlane.CheckState.value__
    End Sub

    Private Sub ButtFree_CheckedChanged(sender As Object, e As EventArgs) Handles ButtFree.CheckedChanged
        If (Me.Component.MyGumball IsNot Nothing) AndAlso (CanSend) Then Component.MyGumball.CustomAppearance(2) = Me.ButtFree.CheckState.value__
    End Sub

    Private Sub ButtRotate_CheckedChanged(sender As Object, e As EventArgs) Handles ButtRotate.CheckedChanged
        If (Me.Component.MyGumball IsNot Nothing) AndAlso (CanSend) Then Component.MyGumball.CustomAppearance(3) = Me.ButtRotate.CheckState.value__
    End Sub

    Private Sub ButtScale_CheckedChanged(sender As Object, e As EventArgs) Handles ButtScale.CheckedChanged
        If (Me.Component.MyGumball IsNot Nothing) AndAlso (CanSend) Then Component.MyGumball.CustomAppearance(4) = Me.ButtScale.CheckState.value__
    End Sub

    Private Sub NumRad_ValueChanged(sender As Object, e As EventArgs) Handles NumRad.ValueChanged
        If (Me.Component.MyGumball IsNot Nothing) AndAlso (CanSend) Then Component.MyGumball.CustomAppearance(5) = CInt(Me.NumRad.Value)
    End Sub

    Private Sub NumAH_ValueChanged(sender As Object, e As EventArgs) Handles NumAH.ValueChanged
        If (Me.Component.MyGumball IsNot Nothing) AndAlso (CanSend) Then Component.MyGumball.CustomAppearance(6) = CInt(Me.NumAH.Value)
    End Sub

    Private Sub NumThk_ValueChanged(sender As Object, e As EventArgs) Handles NumThk.ValueChanged
        If (Me.Component.MyGumball IsNot Nothing) AndAlso (CanSend) Then Component.MyGumball.CustomAppearance(7) = CInt(Me.NumThk.Value)
    End Sub

    Private Sub NumPS_ValueChanged(sender As Object, e As EventArgs) Handles NumPS.ValueChanged
        If (Me.Component.MyGumball IsNot Nothing) AndAlso (CanSend) Then Component.MyGumball.CustomAppearance(8) = CInt(Me.NumPS.Value)
    End Sub

    Private Sub NumPD_ValueChanged(sender As Object, e As EventArgs) Handles NumPD.ValueChanged
        If (Me.Component.MyGumball IsNot Nothing) AndAlso (CanSend) Then Component.MyGumball.CustomAppearance(9) = CInt(Me.NumPD.Value)
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
        Me.NumPD.Minimum = New Decimal(New Integer() {1, 0, 0, 0})
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

Public Class FormTextBox
    Inherits System.Windows.Forms.Form

    Public GB As GhGumball

    Sub New(Loc As System.Drawing.Point, MyOwner As GhGumball)
        GB = MyOwner
        Me.Location = Loc
        InitializeComponent()
        Me.Show()
    End Sub

    Private Sub TextBox1_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox1.KeyDown
        If e.KeyData = Keys.Enter Then
            GB.Component.RecordUndoEvent("Gumball Drag", New GbUndo(GB))
            GB.TransformFromTextBox()
            Me.Dispose()
            Me.Close()
        End If
    End Sub

    Private Sub TextBox1_TextChanged(sender As Object, e As EventArgs) Handles TextBox1.TextChanged
        GB.ValueString = Me.TextBox1.Text
    End Sub

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
        Me.TextBox1 = New System.Windows.Forms.TextBox()
        Me.SuspendLayout()
        '
        'TextBox1
        '
        Me.TextBox1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TextBox1.Location = New System.Drawing.Point(0, 0)
        Me.TextBox1.Name = "TextBox1"
        Me.TextBox1.Size = New System.Drawing.Size(100, 20)
        Me.TextBox1.TabIndex = 0
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(100, 20)
        Me.Controls.Add(Me.TextBox1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None
        Me.StartPosition = FormStartPosition.Manual
        Me.MaximumSize = New System.Drawing.Size(100, 20)
        Me.MinimumSize = New System.Drawing.Size(100, 20)
        Me.Name = "Form1"
        Me.Text = "Form1"
        Me.Owner = Grasshopper.Instances.DocumentEditor
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents TextBox1 As TextBox
End Class