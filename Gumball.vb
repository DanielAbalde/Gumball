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
        Rhino.RhinoApp.WriteLine(Me.Radius)
    End Sub

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
        m_attributes = New GumballAtt(Me)
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

    Protected Overrides Sub AppendAdditionalComponentMenuItems(ByVal menu As Windows.Forms.ToolStripDropDown)

        Dim union As Windows.Forms.ToolStripMenuItem = Menu_AppendItem(menu, "Apply to all", AddressOf Me.ApplyToAll, True, Me.GetValue("gbatt", 0) = 1)
        union.ToolTipText = "Performs transformation of a gumball to all geometry"

        '   Dim sep As Windows.Forms.ToolStripSeparator = Menu_AppendSeparator(menu)

        Dim reloc As Windows.Forms.ToolStripMenuItem = Menu_AppendItem(menu, "Relocate gumball", AddressOf Me.RelocateG, True, Me.GetValue("gbatt", 0) = 2)
        reloc.ToolTipText = "Relocate gumball without affecting the geometry"

        Dim Rad As Windows.Forms.ToolStripMenuItem = Menu_AppendItem(menu, "Radius gumball", AddressOf ResizeGumball, True)
        Rad.ToolTipText = "Changes the gumball radius"

        Dim CC As Windows.Forms.ToolStripMenuItem = Menu_AppendItem(menu, "Reset gumball", AddressOf ClearCache, True)
        CC.ToolTipText = "Reset gumball and clear cache data"

    End Sub

    Protected Overrides Sub ValuesChanged()

        Select Case Me.GetValue("gbatt", 0)
            Case 0
                Me.Message = String.Empty
            Case 1
                Me.Message = "Apply to all"
            Case 2
                Me.Message = "Relocate"
        End Select

        If (MyGumball IsNot Nothing) Then MyGumball.Resize(Me.GetValue("radius", 60))
        Me.RecordUndoEvent("gumballattributes")
        Me.ExpireSolution(True)
    End Sub

    Private Sub ApplyToAll(sender As Object, e As System.EventArgs)
        If (Me.GetValue("gbatt", 0) = 1) Then
            Me.SetValue("gbatt", 0)
        Else
            Me.SetValue("gbatt", 1)
        End If
    End Sub

    Private Sub RelocateG(sender As Object, e As System.EventArgs)
        If (Me.GetValue("gbatt", 0) = 2) Then
            Me.SetValue("gbatt", 0)
        Else
            Me.SetValue("gbatt", 2)
        End If
    End Sub

    Public ReadOnly Property AttValue As Integer
        Get
            Return Me.GetValue("gbatt", 0)
        End Get
    End Property

    Public Property Radius As Integer
        Get
            Return Me.GetValue("radius", 60)
        End Get
        Set(value As Integer)
            Me.SetValue("radius", value)
        End Set
    End Property

    Public RadForm As FormRadius = Nothing

    Private Sub ResizeGumball()
        If (RadForm Is Nothing) Then
            RadForm = New FormRadius(Me)
        Else
            RadForm.Dispose()
        End If
    End Sub

    Public Sub ClearCache()
        Cache.Clear()
        If (MyGumball IsNot Nothing) Then MyGumball.Dispose()
        MyGumball = Nothing
        Me.ClearData()
        Me.ExpireSolution(True)
    End Sub

    'Public Overrides Property Locked As Boolean
    '    Get
    '        Return MyBase.Locked
    '    End Get
    '    Set(value As Boolean)
    '        If (MyGumball IsNot Nothing) AndAlso Not (value) Then MyGumball.HideGumballs()
    '        MyBase.Locked = value
    '    End Set
    'End Property

    'Public Overloads Property Hidden As Boolean
    '    Get
    '        Return MyBase.Hidden
    '    End Get
    '    Set(value As Boolean)

    '        If (MyGumball IsNot Nothing) Then

    '            If (MyBase.Hidden) Then
    '                If (value) Then
    '                    MyGumball.ShowGumballs()
    '                    Rhino.RhinoApp.WriteLine("SHOW HIDDEN")
    '                End If
    '            Else
    '                If Not (value) Then
    '                    MyGumball.HideGumballs()
    '                    Rhino.RhinoApp.WriteLine("DISPOSE HIDDEN")
    '                End If
    '            End If

    '        End If
    '        MyBase.Hidden = value
    '    End Set
    'End Property

    Public MyGumball As GhGumball
    Private Cache As New List(Of GeometryBase)

    Protected Overrides Sub SolveInstance(DA As IGH_DataAccess)

        Dim InputData As New List(Of GeometryBase)
        Dim Data As New List(Of Types.IGH_GeometricGoo)

        'Get input data.
        If Not (DA.GetDataList(0, Data)) Then
            ClearCache()
            Exit Sub
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
        If Not (AreEquals(Cache, InputData)) Then ClearCache()

        'Create Gumball class.
        If (MyGumball Is Nothing) Then
            Dim inputDataFree As GeometryBase() = New GeometryBase(InputData.Count - 1) {}
            For i As Int32 = 0 To InputData.Count - 1
                inputDataFree(i) = InputData(i).Duplicate
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
            Me.SetValue("gbatt", att.GetInt32("val", 0))
            Me.SetValue("radius", att.GetInt32("rad", 1))
        Catch ex As Exception
            Rhino.RhinoApp.WriteLine("READ_COMP_Attributes; " & ex.ToString())
        End Try

        If (gb IsNot Nothing) Then MyGumball = New GhGumball(gb, Me)

        Return MyBase.Read(reader)
    End Function
#End Region

End Class

Public Class GumballAtt
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
            If (comp IsNot Nothing) Then
                app.Radius = comp.Radius
            Else
                app.Radius = 60
            End If
            If (Geo(i).ObjectType = Rhino.DocObjects.ObjectType.Point) Then
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

    Public Sub Resize(ByVal Radius As Integer)

        MyCallBack.Enabled = False
        For i As Int32 = 0 To Count - 1
            Dim app As Rhino.UI.Gumball.GumballAppearanceSettings = Appearances(i)
            If (Radius = app.Radius) Then Exit For
            app.Radius = Radius
            Appearances(i) = app
            Conduits(i).Enabled = False
            Conduits(i).SetBaseGumball(Gumballs(i), Appearances(i))
            Conduits(i).Enabled = True
        Next
        MyCallBack.Enabled = True
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
            att.SetInt32("val", 0, Me.Component.AttValue)
            att.SetInt32("rad", 1, Me.Component.Radius)

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

            Dim readergumball As New GhGumball(Geom, Nothing)
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

            If (G.Component.AttValue = 2) Then
                G.UpdateGumball(Index)
            Else

                Dim ghXform As New Grasshopper.Kernel.Types.GH_Transform(New Grasshopper.Kernel.Types.Transforms.Generic(xform))

                If (G.Component.AttValue = 1) Then

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

Public Class FormRadius
    Inherits System.Windows.Forms.Form

    Private Component As GumballComp
    Private components As System.ComponentModel.IContainer
    Friend WithEvents NumericUpDown1 As NumericUpDown

    Sub New(Comp As GumballComp)
        Component = Comp
        InitializeComponent()
        Me.Show()
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

    Private Sub InitializeComponent()
        Me.NumericUpDown1 = New System.Windows.Forms.NumericUpDown()
        CType(Me.NumericUpDown1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'NumericUpDown1
        '
        Me.NumericUpDown1.Location = New System.Drawing.Point(2, 3)
        Me.NumericUpDown1.Maximum = New Decimal(New Integer() {200, 0, 0, 0})
        Me.NumericUpDown1.Minimum = New Decimal(New Integer() {20, 0, 0, 0})
        Me.NumericUpDown1.Name = "NumericUpDown1"
        Me.NumericUpDown1.Size = New System.Drawing.Size(120, 20)
        Me.NumericUpDown1.TabIndex = 0
        Me.NumericUpDown1.Value = New Decimal(New Integer() {60, 0, 0, 0})
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(124, 25)
        Me.Controls.Add(Me.NumericUpDown1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "Form1"
        Me.ShowIcon = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Owner = Grasshopper.Instances.DocumentEditor
        Me.Text = "Gumball Radius"
        CType(Me.NumericUpDown1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub

    Private Sub NumericUpDown1_ValueChanged(sender As Object, e As EventArgs) Handles NumericUpDown1.ValueChanged
        Me.Component.Radius = CInt(Me.NumericUpDown1.Value)
    End Sub

    Private Sub FormRadius_FormClosed(sender As Object, e As FormClosedEventArgs) Handles MyBase.FormClosed
        Me.Component.RadForm = Nothing
    End Sub

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Me.NumericUpDown1.Value = New Decimal(New Integer() {Me.Component.Radius, 0, 0, 0})
    End Sub

End Class
