/*
 *  Project:        Membership Log (application using WhaTBI?)
 *  Filename:       MainForm.cs
 *  Description:    Main form and app entry point
 */

using System;
using System.Collections;
using System.Diagnostics;
using System.Drawing;
using System.IO;
using System.Reflection;
using System.Windows.Forms;

using Pdbartlett.Whatbi;
using Pdbartlett.Whatbi.Expression;

// Assembly metadata
[assembly: AssemblyTitle("MembershipLog")]
[assembly: AssemblyDescription("Membership Log application based on WhaTBI? framework")]
[assembly: AssemblyCompany("Paul D. Bartlett")]
[assembly: AssemblyProduct("MembershipLog")]
[assembly: AssemblyCopyright("Copyright © Paul D. Bartlett 2004-2006")]
[assembly: AssemblyVersion("0.1.0.1")]

namespace Pdbartlett.MembershipLog
{
    public class EntryPoint
    {
        [STAThread]
        public static void Main(string[] args)
        {
            // TODO: surround this call by appropriate exception handling
            ShowMainForm(args);
        }

        private static void ShowMainForm(string[] args)
        {
            Application.Run(new MainForm(args));
        }
    }

    delegate void Action();

    class MainForm : GridForm
    {
        // Readonly and constant values
        private readonly Object EXAMPLE_ITEM            = new Member();
        private readonly Type   ITEM_TYPE               = typeof(Member);
        private readonly Type   ITEM_ARRAY_TYPE         = typeof(Member[]);
        private readonly Type[] EXTRA_TYPES             = new Type[] { typeof(Detail), typeof(Membership), typeof(Payment) };

        private const string    DEFAULT_FILE_NAME       = "MemberData.xml";
        private const string    SETTINGS_FILE_NAME      = "Settings.xml";
        private const string    FILTER_UNNAMED          = "<unnamed>";

        private readonly string DEFAULT_FOLDER =
            Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "MembershipLog");

        // String constants (should be moved out for globalisation)
        private const string    MENU_FILE               = "&File";
        private const string    MENU_FILE_SAVE          = "&Save";
        private const string    MENU_FILE_BACKUP        = "&Backup";
        private const string    MENU_FILE_RESTORE       = "&Restore";
        private const string    MENU_FILE_EXPORT        = "&Export";
        private const string    MENU_FILE_EXPORT_CSV    = "Current view as &CSV";
        private const string    MENU_FILE_EXIT          = "E&xit";
        private const string    MENU_VIEW               = "&View";
        private const string    MENU_VIEW_SORT          = "&Sort";
        private const string    MENU_VIEW_SIMPLE        = "Simple &filter...";
        private const string    MENU_VIEW_ADVANCED      = "Ad&vanced filter...";
        private const string    MENU_VIEW_SAVEAS        = "Save filter &as...";
        private const string    MENU_VIEW_DELETE        = "&Delete filter";
        private const string    MENU_VIEW_CLEAR         = "&Clear filter";
        private const string    MENU_TOOLS              = "&Tools";
        private const string    MENU_TOOLS_OPTIONS      = "&Options";
        private const string    MENU_TOOLS_GENERATE     = "&Generate data...";
        private const string    MENU_HELP               = "&Help";
        private const string    MENU_HELP_CONTENTS      = "&Contents...";
        private const string    MENU_HELP_ABOUT         = "&About...";
        private const string    MENU_COLUMN_SORTBY      = "&Sort by";
        private const string    MENU_COLUMN_MOVELEFT    = "Move &left";
        private const string    MENU_COLUMN_MOVERIGHT   = "Move &right";
        private const string    MENU_COLUMN_HIDE        = "&Hide";
        private const string    MENU_COLUMN_ADD         = "&Add...";

        // Runtime flags
        bool                    m_debugFlag;

        // Data members
        UpdatableSortDecorator  m_ds;
        string                  m_filePath;
        string                  m_filterName;
        IPredicate              m_currentFilter;

        // Controls
        DataGrid    m_dgMembers;
        MainMenu    m_menu;
        MenuItem    m_fileMenu;
        MenuItem    m_recordMenu;
        MenuItem    m_viewMenu;
        MenuItem    m_toolsMenu;
        MenuItem    m_helpMenu;

        public MainForm(string[] args)
        {
            // Ensure "default folder" exists (needed for settings and possibly the data file)
            if (!Directory.Exists(DEFAULT_FOLDER))
                Directory.CreateDirectory(DEFAULT_FOLDER);

            int cArgs = args.Length;

            if (cArgs > 0 && args[0].ToLower() == "-debug")
                m_debugFlag = true;

            if (cArgs > (m_debugFlag ? 1 : 0))
                m_filePath = args[cArgs - 1];
            else
                m_filePath = Path.Combine(DEFAULT_FOLDER, DEFAULT_FILE_NAME);

            Visible = false;
        }

        protected override void OnLoad(EventArgs ea)
        {
            Text = "Loading...";
            Cursor = Cursors.WaitCursor;

            base.OnLoad(ea);

            // Main Form
            Rectangle screen = Screen.FromControl(this).Bounds;
            Rectangle rect = new Rectangle
                (   screen.Left + screen.Width / 6
                ,   screen.Top + screen.Height / 6
                ,   2 * screen.Width / 3
                ,   2 * screen.Height / 3
                );
            DesktopBounds = rect;

            // Menus

            m_fileMenu = new MenuItem(MENU_FILE);
            m_fileMenu.MenuItems.Add(new MenuItem(MENU_FILE_SAVE, new EventHandler(File_Save), Shortcut.CtrlS));
            m_fileMenu.MenuItems.Add(new MenuItem(MENU_FILE_BACKUP, new EventHandler(File_Backup)));
            m_fileMenu.MenuItems.Add(new MenuItem(MENU_FILE_RESTORE, new EventHandler(File_Restore)));
            MenuItem fileExportMenu = new MenuItem(MENU_FILE_EXPORT);
            fileExportMenu.MenuItems.Add(new MenuItem(MENU_FILE_EXPORT_CSV, new EventHandler(Export_Csv)));
            m_fileMenu.MenuItems.Add(fileExportMenu);
            m_fileMenu.MenuItems.Add(new MenuItem(MENU_FILE_EXIT, new EventHandler(File_Exit), Shortcut.AltF4));

            m_recordMenu = new MenuItem(MENU_RECORD);
            m_recordMenu.Popup += new EventHandler(RecordMenu_Popup);
            m_recordMenu.MenuItems.Add(new MenuItem(MENU_RECORD_NEW,    new EventHandler(Action_New), Shortcut.CtrlN));
            m_recordMenu.MenuItems.Add(new MenuItem(MENU_RECORD_VIEW,   new EventHandler(Action_View)));
            m_recordMenu.MenuItems.Add(new MenuItem(MENU_RECORD_UPDATE, new EventHandler(Action_Update)));
            m_recordMenu.MenuItems.Add(new MenuItem(MENU_RECORD_DELETE, new EventHandler(Action_Delete), Shortcut.CtrlD));

            LoadSettings();
            BuildViewMenu();

            m_toolsMenu = new MenuItem(MENU_TOOLS);
            m_toolsMenu.MenuItems.Add(new MenuItem(MENU_TOOLS_OPTIONS,  new EventHandler(Tools_Options), Shortcut.CtrlO));
            if (m_debugFlag)
            {
                m_toolsMenu.MenuItems.Add("-");
                m_toolsMenu.MenuItems.Add(new MenuItem(MENU_TOOLS_GENERATE, new EventHandler(Tools_Generate)));

            }

            m_helpMenu = new MenuItem(MENU_HELP);
            m_helpMenu.MenuItems.Add(new MenuItem(MENU_HELP_CONTENTS,   new EventHandler(Help_Contents), Shortcut.F1));
            m_helpMenu.MenuItems.Add(new MenuItem(MENU_HELP_ABOUT,      new EventHandler(Help_About)));

            m_menu = new MainMenu(
                new MenuItem[] { m_fileMenu, m_recordMenu, m_viewMenu, m_toolsMenu, m_helpMenu }
            );

            Menu = m_menu;

            ColumnPopup = new ContextMenu();
            ColumnPopup.MenuItems.Add(new MenuItem(MENU_COLUMN_SORTBY,      new EventHandler(Column_SortBy)));
            ColumnPopup.MenuItems.Add(new MenuItem(MENU_COLUMN_MOVELEFT,    new EventHandler(Column_MoveLeft)));
            ColumnPopup.MenuItems.Add(new MenuItem(MENU_COLUMN_MOVERIGHT,   new EventHandler(Column_MoveRight)));
            ColumnPopup.MenuItems.Add(new MenuItem(MENU_COLUMN_HIDE,        new EventHandler(Column_Hide)));
            ColumnPopup.MenuItems.Add(new MenuItem(MENU_COLUMN_ADD,         new EventHandler(Column_Add)));

            // Members data grid
            m_dgMembers = new DataGrid();
            m_dgMembers.Dock = DockStyle.Fill;
            SetStandardGridProperties(m_dgMembers);
            Settings.Current.Columns.ApplyToGrid(m_dgMembers);
            Controls.Add(m_dgMembers);

            // Make form visible
            Visible = true;

            // Connect to data source and load data
            CreateDataSource();

            // Restore cursor and set correct title
            Text = "Membership Log";
            Cursor = Cursors.Default;
        }

        protected override void OnClosed(EventArgs ea)
        {
            DetachFromDataSource(new Action(SaveSettings));
            base.OnClosed(ea);
        }

        private void DetachFromDataSource(params Action[] actions)
        {
            string title = Text;

            Text = "Saving...";
            Cursor = Cursors.WaitCursor;

            IDisposable disp = m_ds as IDisposable;
            if (disp != null)
                disp.Dispose();

            Text = "Working...";

            foreach (Action action in actions)
            {
                action();
            }

            Text = title;
            Cursor = Cursors.Default;
        }

        protected override object GetNewObject(IUpdatableDataSource ds)
        {
            return new Member();
        }

        protected override Form GetActionForm(object o, RecordAction action)
        {
            return new MemberForm((Member)o, action);
        }

        protected override void ActionCompleted(IUpdatableDataSource ds)
        {
            RefreshView();
        }

        private void Export_Csv(object sender, EventArgs ea)
        {
            string title = Text;

            Text = "Exporting...";
            Cursor = Cursors.WaitCursor;

            ArrayList mappings = new ArrayList();
            SaveFileDialog sfd = new SaveFileDialog();
            sfd.Filter = "CSV files (*.csv)|*.csv|Text files (*.txt)|*.txt|All files (*.*)|*.*";
            sfd.FilterIndex = 1;
            sfd.DefaultExt = ".csv";
            sfd.AddExtension = true;

            if (sfd.ShowDialog() == DialogResult.OK)
            {
                using (Stream strm = sfd.OpenFile())
                using (StreamWriter sw = new StreamWriter(strm))
                {
                    GridColumnStylesCollection cols = m_dgMembers.TableStyles[0].GridColumnStyles;
                    foreach (DataGridColumnStyle col in cols)
                    {
                        sw.Write("\"" + col.HeaderText + "\",");
                        mappings.Add(Expr.GetProperty(col.MappingName));
                    }

                    sw.Write(Environment.NewLine);

                    foreach (object oItem in (ICollection)CurrentData)
                    {
                        foreach(object oExpr in mappings)
                        {
                            IExpression expr = (IExpression)oExpr;
                            String s = "\"\"";
                            try
                            {
                                Object o = expr.Evaluate(oItem);
                                if (o is String)
                                {
                                    String src = ((String)o).Replace("\r", "").Replace("\n", " ");
                                    s = "\"" + src.Replace("\"", "\\\"") + "\"";
                                }
                                else
                                {
                                    s = o.ToString();
                                }
                            }
                            catch (Exception /*e*/)
                            {
                                // Property not found - ignore
                            }
                            sw.Write(s + ",");
                        }

                        sw.Write(Environment.NewLine);
                    }
                }
            }

            Text = title;
            Cursor = Cursors.Default;

            if (MessageBox.Show("Do you want to open the exported data now?", "Membership Log",
                                MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                try
                {
                    Process.Start(sfd.FileName);
                }
                catch (Exception e)
                {
                    MessageBox.Show("Failed to open file automatically; please try manually instead. Reason: " +
                                    e.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                }
            }
        }

        private void Column_SortBy(object sender, EventArgs ea)
        {
            DataGridColumnStyle col = GetColumn();
            if (col == null)
                return;

            string propName = col.MappingName;

            if (m_ds.SortSpec != null && m_ds.SortSpec.PropName == propName)
                m_ds.SortSpec.ToggleAscending();
            else
                m_ds.SortSpec = new SortSpec(propName);

            RefreshView();
        }

        private void Column_MoveLeft(object sender, EventArgs ea)
        {
            MoveColumn(-1);
        }

        private void Column_MoveRight(object sender, EventArgs ea)
        {
            MoveColumn(1);
        }

        private void Column_Hide(object sender, EventArgs ea)
        {
            DataGridColumnStyle col = GetColumn();
            if (col == null)
                return;

            // As we've got the column style, we can safely get the collection
            GridColumnStylesCollection cols = m_dgMembers.TableStyles[0].GridColumnStyles;

            if (cols.Count == 1)
            {
                MessageBox.Show("Grid must contain at least one column", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                return;
            }

            cols.Remove(col);
        }

        private void Column_Add(object sender, EventArgs ea)
        {
            DataGridColumnStyle clicked = GetColumn();
            if (clicked == null)
                return;

            // As we've got the column style, we can safely get the collection that we'll need later
            GridColumnStylesCollection cols = m_dgMembers.TableStyles[0].GridColumnStyles;

            Hashtable possibleCols = new Hashtable();

            foreach (IPropertyInfo pi in (Utility.GetTypeInfo(EXAMPLE_ITEM)).GetProperties())
            {
                if (!pi.TypeInfo.IsCollection)
                    possibleCols.Add(pi.Name, pi);
            }

            foreach (object o in cols)
            {
                DataGridColumnStyle col = (DataGridColumnStyle)o;
                possibleCols.Remove(col.MappingName);
            }

            IPropertyInfo newProp = null;

            using (ListSelectionForm lsf = new ListSelectionForm(possibleCols.Values))
            {
                if (lsf.ShowDialog() == DialogResult.OK)
                    newProp = lsf.SelectedItem as IPropertyInfo;
            }

            if (newProp == null)
                return;

            DataGridColumnStyle newCol = new DataGridTextBoxColumn();
            newCol.MappingName = newProp.Name;
            newCol.HeaderText = newProp.DisplayName;
            cols.Add(newCol);
        }

        private void MoveColumn(int offset)
        {
            DataGridColumnStyle col = GetColumn();
            if (col == null)
                return;

            // As we've got the column style, we can safely get the collection
            GridColumnStylesCollection cols = m_dgMembers.TableStyles[0].GridColumnStyles;

            // Validate offset
            int cCols = cols.Count;
            int newPos = HitCol + offset;
            if (newPos < 0 || newPos >= cCols)
                return;

            DataGridColumnStyle[] temp = new DataGridColumnStyle[cCols];
            cols.CopyTo(temp, 0);
            cols.Clear();

            // first copy up to the preceding column
            for (int i = 0; i < (HitCol - 1); ++i)
                cols.Add(temp[i]);

            // then copy the next three columns in the "new" order
            if (offset == -1)
            {
                cols.Add(col);
                cols.Add(temp[HitCol - 1]);
                if (HitCol < cCols - 1)
                    cols.Add(temp[HitCol + 1]);
            }
            else
            {
                if (HitCol > 0)
                    cols.Add(temp[HitCol - 1]);
                cols.Add(temp[HitCol + 1]);
                cols.Add(col);
            }

            // then copy the rest
            for (int i = HitCol + 2; i < cCols; ++i)
                cols.Add(temp[i]);
       }

        DataGridColumnStyle GetColumn()
        {
            if (HitCol < 0)
                return null;

            if (m_dgMembers.TableStyles.Count != 1)
                return null;

            DataGridTableStyle tableStyle = m_dgMembers.TableStyles[0];

            if (tableStyle.GridColumnStyles.Count <= HitCol)
                return null;

            return tableStyle.GridColumnStyles[HitCol];
        }

        private void File_Backup(object sender, EventArgs ea)
        {
            DetachFromDataSource(new Action(BackupData), new Action(CreateDataSource));
        }

        private void File_Restore(object sender, EventArgs ea)
        {
            DetachFromDataSource(new Action(RestoreData), new Action(CreateDataSource));
        }

        private void File_Save(object sender, EventArgs ea)
        {
            DetachFromDataSource(new Action(SaveSettings), new Action(CreateDataSource));
        }

        private void File_Exit(object sender, EventArgs ea)
        {
            Close();
        }

        private void Help_Contents(object sender, EventArgs ea)
        {
            Help.ShowHelp(this, Application.ExecutablePath.Replace(".exe", ".chm"));
        }

        private void Help_About(object sender, EventArgs ea)
        {
            MessageBox.Show("An application based on the WhaTBI? framework\n\n" +
                             "Copyright © 2004-2006 Paul D. Bartlett",
                             "Membership Log v" + Assembly.GetExecutingAssembly().GetName().Version,
                             MessageBoxButtons.OK,
                             MessageBoxIcon.Information);
        }

        private void Tools_Options(object sender, EventArgs ea)
        {
            using (SettingsForm form = new SettingsForm())
            {
                form.ShowDialog();
            }

            // Rebuild properties (example item and real data) if dynamic, in case they've changed
            // TODO - make this a little more lightweight, and only if really necessary
            DynamicObjectHelper dyn = EXAMPLE_ITEM as DynamicObjectHelper;
            if (dyn != null)
            {
                dyn.RebuildProperties();
                DetachFromDataSource(new Action(CreateDataSource));
            }
        }

        private void Tools_Generate(object sender, EventArgs ea)
        {
            using (InputBox form = new InputBox("Generate data", "How many members?", "100",
                                                 new StringValidator(NumberValidator)))
            {
                if (form.ShowDialog() != DialogResult.OK)
                    return;

                int cMembers;
                if (!Utility.TryParseInt32(form.Value, out cMembers))
                    return;

                int id = Updatable.Count;

                string title = Text;
                Cursor = Cursors.WaitCursor;

                for (int i = 0; i < cMembers; ++i)
                {
                    Text = String.Format("Generating data... {0}/{1}", i + 1, cMembers);
                    Updatable.Insert(Member.GenerateRandom(String.Format("ABC{0:000000}", ++id)));
                }

                Text = "Refreshing view...";
                RefreshView();

                Text = title;
                Cursor = Cursors.Default;
            }
        }

        private bool NumberValidator(string text)
        {
            int dummy;
            return Utility.TryParseInt32(text, out dummy);
        }

        private void RecordMenu_Popup(object sender, EventArgs ea)
        {
            UpdateRecordMenu(m_recordMenu);
        }

        private void ViewMenu_Popup(object sender, EventArgs ea)
        {
            UpdateViewMenu();
        }

        private void View_Sort(object sender, EventArgs ea)
        {
            using (SortForm form = new SortForm(Utility.GetTypeInfo(EXAMPLE_ITEM), m_ds.SortSpec))
            {
                if (form.ShowDialog() != DialogResult.OK)
                    return;

                m_ds.SortSpec = form.SortSpec;
                RefreshView();
            }
        }

        private void View_Simple(object sender, EventArgs ea)
        {
            ShowFilterForm(new FilterForm(Utility.GetTypeInfo(EXAMPLE_ITEM), "Define simple filter"));
        }

        private void View_Advanced(object sender, EventArgs ea)
        {
            ShowFilterForm(new MultiFilterForm(Utility.GetTypeInfo(EXAMPLE_ITEM)));
        }

        private void ShowFilterForm(BaseFilterForm form)
        {
            using (form)
            {
                if (form.ShowDialog() == DialogResult.OK)
                {
                    FilterSpec filter = form.Filter;
                    if (filter != null)
                        ApplyFilter(form.Filter);
                }
            }
        }

        private void View_SaveAs(object sender, EventArgs ea)
        {
            using (InputBox form = new InputBox("Enter name for filter", "Name", m_filterName,
                                                 new StringValidator(FilterNameValidator)))
            {
                if (form.ShowDialog() == DialogResult.OK)
                {
                    string name = form.Value;
                    IDictionary filters = Settings.Current.Filters;

                    if (filters.Contains(name) &&
                        MessageBox.Show("A filter with that name already exists. Overwrite?", "Confirm overwrite",
                            MessageBoxButtons.YesNo, MessageBoxIcon.Question, MessageBoxDefaultButton.Button2)
                            == DialogResult.No)
                    {
                        return;
                    }

                    filters[name] = m_currentFilter;
                    BuildViewMenu();
                    ApplyFilter(m_currentFilter, name);
                }
            }
        }

        private void View_Select(object sender, EventArgs ea)
        {
            MenuItem mi = sender as MenuItem;
            if (mi == null)
                return;

            IPredicate filter = Settings.Current.Filters[mi.Text] as IPredicate;
            if (filter == null)
                return;

            ApplyFilter(filter, mi.Text);
        }

        private void View_Delete(object sender, EventArgs ea)
        {
            if (MessageBox.Show("Delete saved filter '" + m_filterName + "'?", "Confirm delete",
                                 MessageBoxButtons.YesNo, MessageBoxIcon.Question,
                                 MessageBoxDefaultButton.Button2) == DialogResult.No)
            {
                return;
            }

            Settings.Current.Filters.Remove(m_filterName);
            BuildViewMenu();

            ApplyFilter(m_currentFilter, null);
        }

        private void View_Clear(object sender, EventArgs ea)
        {
            ApplyFilter(null, null);
        }

        private void UpdateViewMenu()
        {
            foreach (MenuItem mi in m_viewMenu.MenuItems)
            {
                switch (mi.Text)
                {
                    case MENU_VIEW_SORT:
                    case MENU_VIEW_SIMPLE:
                    case MENU_VIEW_ADVANCED:
                        mi.Enabled = true;
                        break;
                    case MENU_VIEW_SAVEAS:
                    case MENU_VIEW_CLEAR:
                        mi.Enabled = (m_currentFilter != null);
                        break;
                    case MENU_VIEW_DELETE:
                        mi.Enabled = (m_filterName != null && m_filterName != FILTER_UNNAMED);
                        break;
                }
            }
        }

        private bool FilterNameValidator(string name)
        {
            return name != FILTER_UNNAMED;
        }

        private void ApplyFilter(FilterSpec filter)
        {
            if (filter == null)
                return;

            string desc = filter.Description;
            if (desc == null)
                desc = FILTER_UNNAMED;

            ApplyFilter(filter.Predicate, desc);
        }

        private void ApplyFilter(IPredicate predicate, string desc)
        {
            FilterHelper(predicate, desc, false);
        }

        private void RefreshView()
        {
            FilterHelper(m_currentFilter, null, true);
        }

        private void FilterHelper(IPredicate filter, string desc, bool forcedRefresh)
        {
            if (forcedRefresh || filter == null || m_currentFilter != filter)
            {
                m_currentFilter = filter;
                SetCurrentData(filter == null ? m_ds.GetAll() : m_ds.Query(filter));
            }

            if (filter == null)
            {
                m_filterName = null;
                m_dgMembers.CaptionText = String.Format("All records ({0})", m_ds.Count);
            }
            else
            {
                m_filterName = (desc == null) ? FILTER_UNNAMED : desc;
                m_dgMembers.CaptionText = String.Format("Filtered by: {0} ({1}/{2})",
                    m_filterName, ((ICollection)CurrentData).Count, m_ds.Count);
            }
        }

        private void SetCurrentData(ICollection newData)
        {
            CurrentData = newData;
            m_dgMembers.DataSource = newData;
            m_dgMembers.Refresh();
        }

        private Member GetCurrentMember()
        {
            return (Member)GetCurrent();
        }

        private void CreateDataSource()
        {
            m_ds =
                new UpdatableSortDecorator(
                    new CachingBulkFileAdapter(
                        new SerializedXmlDataSource(ITEM_TYPE, ITEM_ARRAY_TYPE, EXTRA_TYPES, m_filePath, FileShare.None)
                    )
                )
            ;

            CurrentDataSource = m_ds;
            RefreshView();
        }

        private void BackupData()
        {
            string filePath = GetBackupRestorePath(true);
            if (filePath != null)
                File.Copy(m_filePath, filePath, true);
        }

        private void RestoreData()
        {
            string filePath = GetBackupRestorePath(false);
            if (filePath != null)
            {
                if (MessageBox.Show("This will permanently overwrite the current data with that from the chosen backup file. " +
                                    "Are you sure you want to continue?", "Overwrite data?",
                                    MessageBoxButtons.YesNo, MessageBoxIcon.Question,
                                    MessageBoxDefaultButton.Button2) == DialogResult.Yes)
                {
                    File.Copy(filePath, m_filePath, true);
                }
            }
        }

        private string GetBackupRestorePath(bool isBackup)
        {
            FileDialog fd;
            if (isBackup)
            {
                fd = new SaveFileDialog();
            }
            else
            {
                OpenFileDialog ofd = new OpenFileDialog();
                ofd.CheckFileExists = true;
                fd = ofd;
            }

            fd.Filter = "XML files (*.xml)|*.xml|All files (*.*)|*.*";
            fd.FilterIndex = 1;

            string filePath = null;

            for(;;)
            {
                if (fd.ShowDialog() != DialogResult.OK)
                    break;

                if (fd.FileName != m_filePath)
                {
                    filePath = fd.FileName;
                    break;
                }

                string operation = isBackup ? "backup to" : "restore from";
                MessageBox.Show(String.Format("Cannot {0} the main application data file - please choose another location.", operation),
                                "Membership Log", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
            }

            return filePath;
        }

        private string SettingsFile { get { return Path.Combine(DEFAULT_FOLDER, SETTINGS_FILE_NAME); } }

        private void SaveSettings()
        {
            Settings.Save(SettingsFile, m_dgMembers);
        }

        private void LoadSettings()
        {
            bool bDone = false;

            try
            {
                if (File.Exists(SettingsFile))
                {
                    Settings.Load(SettingsFile);
                    bDone = true;
                }
            }
            catch (Exception)
            {
                MessageBox.Show("Failed to restore saved settings - reverting to defaults",
                                "Warning", MessageBoxButtons.OK, MessageBoxIcon.Warning);
            }

            if (!bDone)
            {
                // No file, or failed to load
                Settings.CreateNew(Utility.GetTypeInfo(ITEM_TYPE));
            }
        }

        private void BuildViewMenu()
        {
            if (m_viewMenu == null)
            {
                m_viewMenu = new MenuItem(MENU_VIEW);
                m_viewMenu.Popup += new EventHandler(ViewMenu_Popup);
            }
            else
                m_viewMenu.MenuItems.Clear();

            m_viewMenu.MenuItems.Add(new MenuItem(MENU_VIEW_SORT,       new EventHandler(View_Sort)));
            m_viewMenu.MenuItems.Add("-");
            m_viewMenu.MenuItems.Add(new MenuItem(MENU_VIEW_SIMPLE,     new EventHandler(View_Simple),      Shortcut.CtrlF));
            m_viewMenu.MenuItems.Add(new MenuItem(MENU_VIEW_ADVANCED,   new EventHandler(View_Advanced),    Shortcut.CtrlShiftF));
            m_viewMenu.MenuItems.Add(new MenuItem(MENU_VIEW_CLEAR,      new EventHandler(View_Clear),       Shortcut.CtrlX));
            m_viewMenu.MenuItems.Add(new MenuItem(MENU_VIEW_SAVEAS,     new EventHandler(View_SaveAs),      Shortcut.CtrlS));
            m_viewMenu.MenuItems.Add(new MenuItem(MENU_VIEW_DELETE,     new EventHandler(View_Delete)));

            if (Settings.Current.Filters.Count > 0)
                m_viewMenu.MenuItems.Add("-");

            foreach (DictionaryEntry de in Settings.Current.Filters)
            {
                m_viewMenu.MenuItems.Add(new MenuItem(de.Key.ToString(), new EventHandler(View_Select)));
            }
        }
    }
}