/*
 *  Project:        Membership Log (application using WhaTBI?)
 *  Filename:       Member.cs
 *  Description:    Class describing a member in the app
 */

using System;
using System.Collections;
using System.ComponentModel;

using Pdbartlett.Whatbi;
using Pdbartlett.Whatbi.Custom;

namespace Pdbartlett.MembershipLog
{
    public class Member : DynamicObjectHelper, ICloneable, IGetKey
    {
        // value types
        private bool                    m_doneGymInduction  = false;
        private DateTime                m_dateOfBirth;
        
        // scalar ref types
        private string                  m_id                = "";
        private string                  m_lastName          = "";
        private string                  m_firstName         = "";
        private string                  m_homeAddress       = "";
        private string                  m_department        = "";

        // collections
        private DetailCollection        m_details           = new DetailCollection();
        private MembershipCollection    m_memberships       = new MembershipCollection();
        private PaymentCollection       m_payments          = new PaymentCollection();
        
        public string Id
        {
            get { return m_id; }
            set { m_id = value; }
        }

        public string LastName
        {
            get { return m_lastName; }
            set { m_lastName = value; }
        }

        public string FirstName
        {
            get { return m_firstName; }
            set { m_firstName = value; }
        }
        
        public DateTime DateOfBirth
        {
            get { return m_dateOfBirth; }
            set { m_dateOfBirth = value; }
        }
        
        public int Age
        {
            get
            {
                // !!! approximation !!!
                TimeSpan ts = DateTime.Today - m_dateOfBirth;
                return (int)(Math.Floor(ts.Days / 365.25));
            }
        }
        
        public string HomeAddress
        {
            get { return m_homeAddress; }
            set { m_homeAddress = value; }
        }
        
        public string Department
        {
            get { return m_department; }
            set { m_department = value; }
        }
        
        public bool DoneGymInduction
        {
            get { return m_doneGymInduction; }
            set { m_doneGymInduction = value; }
        }

        public DetailCollection Details
        {
            get { return m_details; }
        }

        public MembershipCollection Memberships
        {
            get { return m_memberships; }
        }

        public PaymentCollection Payments
        {
            get { return m_payments; }
        }
        
        public double OwedCalc
        {
            get
            {
                Decimal owed = 0;
                
                foreach (object o in Memberships)
                {
                    Membership m = (Membership)o;
                    owed += m.Cost;
                }
                
                foreach (object o in Payments)
                {
                    Payment p = (Payment)o;
                    owed -= p.Amount;
                }
                
                return (double)owed;
            }
        }
        
        public string Owed
        {
            get
            {
                return String.Format("{0:0.00}", OwedCalc);
            }
        }
        
        public DateTime CurrentEndDate
        {
            get
            {
                DateTime max = DateTime.MinValue;
                
                foreach (object o in Memberships)
                {
                    Membership m = (Membership)o;
                    if (m.EndDate > max)
                        max = m.EndDate;
                }
                
                return max;
            }
        }
        
        protected override PropertyDescriptorCollection InternalGetPropDescColl()
        {
            PropertyDescriptorCollection pdc    = base.InternalGetPropDescColl();
            DetailTypeCollection dtc            = Settings.Current.Details;
            PropertyDescriptor[] pda            = new PropertyDescriptor[pdc.Count + dtc.Count];
            
            pdc.CopyTo(pda, 0);
            
            int i = pdc.Count;
            foreach (object o in Settings.Current.Details)
            {
                DetailType d = (DetailType)o;
                
                // for all detail values are strings
                pda[i++] = new PropInfoToPropDescAdapter(
                                GetType(),
                                new DynamicPropertyInfo(d.Name,
                                                        new TypeToTypeInfoAdapter(typeof(string)),
                                                        new ObjectValueGetter(DetailGetter)));
            }
            
            return new PropertyDescriptorCollection(pda);
        }
        
        private static object DetailGetter(IPropertyInfo sender, object o)
        {
            Member member = (Member)o;
            Detail detail = member.Details.Get(sender.Name) as Detail;
            return detail == null ? "" : detail.Value;
        }
        
        public object Clone()
        {
            Member m = (Member)MemberwiseClone();
            
            // must use member variables as the corresponding properties are read-only
            m.m_details     = new DetailCollection(Details.GetAll());
            m.m_memberships = new MembershipCollection(Memberships.GetAll());
            m.m_payments    = new PaymentCollection(Payments.GetAll());
            
            return m;
        }
            
        public override bool Equals(object otherObject)
        {
            Member otherMember = otherObject as Member;
            
            return otherMember              != null
                && otherMember.Id           == Id
                && otherMember.LastName     == LastName
                && otherMember.FirstName    == FirstName
                && otherMember.DateOfBirth  == DateOfBirth
                && otherMember.HomeAddress  == HomeAddress
                && otherMember.Department   == Department
            ;
        }
            
        public override int GetHashCode()
        {
            return Id.GetHashCode();
        }
            
        public object GetKey()
        {
            return Id;
        }
        
        internal static Member GenerateRandom(string id)
        {
            Member random = new Member();
            
            random.Id           = id;
            random.LastName     = (string)RandomListElement(lastNames);
            random.FirstName    = (string)RandomListElement(firstNames);
            random.Department   = (string)RandomListElement(deptNames);
            
            int yearsOld = 16 + Random0toN(20) + Random0toN(20) + Random0toN(20);
            int daysOld = 365 * yearsOld + Random0toN(365);
            random.DateOfBirth = DateTime.Today - new TimeSpan(daysOld, 0, 0, 0);
            
            string addressLine1 = String.Format("{0} {1} {2}",
                Random1toN(99), RandomListElement(roadNames), RandomListElement(roadTypes));
                
            if (Random1toN(10) > 6)
            {
                random.HomeAddress = String.Format("{0}\n{1}\nBristol\nBS{2} {3}{4}{5}",
                    addressLine1, RandomListElement(bristolAreas), Random1toN(99), Random0toN(9),
                    RandomLetter(), RandomLetter());
            }
            else
            {
                random.HomeAddress = String.Format("{0}\n{1}\nBath\nBA1 {2}{3}{4}",
                    addressLine1, RandomListElement(bathAreas), Random0toN(9),
                    RandomLetter(), RandomLetter());
            }
            
            foreach (object o in Settings.Current.Details.GetAll())
            {
                if (Random0toN(10) < 8)
                {
                    Detail detail = new Detail();
                    detail.Name = ((DetailType)o).Name;
                    detail.Value = "Test";
                    
                    random.Details.Add(detail);
                }
            }
            
            int cMaxYears = Math.Min(10, random.Age - 15);
            int cYears = Random1toN(cMaxYears);
            int thisYear = DateTime.Today.Year;
            for (int i = 0; i < cYears; ++i)
            {
                Membership ms = new Membership();
                ms.StartDate = new DateTime(thisYear - i, 1, 1);
                ms.EndDate = new DateTime(thisYear - i, 12, 31);
                ms.Cost = (Decimal)(20 - i + 0.50);
                ms.MembershipType = "Standard";
                
                random.Memberships.Add(ms);
                
                int r = Random1toN(20);
                if (i ==0 && r == 1)
                    continue;
                
                Payment p = new Payment();
                p.PaymentDate = ms.StartDate + new TimeSpan(Random0toN(90), 0, 0, 0);
                Decimal proportion = (Decimal)((i == 0 && r == 2) ? Random1toN(12) / 12.0 : 1);
                p.Amount = Decimal.Round(proportion * ms.Cost, 2);
                p.Description = "Cash";
                
                random.Payments.Add(p);
            }
            
            return random;
        }
        
        private static object RandomListElement(IList list)
        {
            return list[Random0toN(list.Count - 1)];
        }
        
        private static int Random0toN(int n)
        {
            if (n < 0)
                throw new ArgumentException();
            
            int val = -1;
            while (val < 0 || val > n)
                val = rand.Next(n + 1);
            
            return val;
        }
        
        private static int Random1toN(int n)
        {
            return 1 + Random0toN(n - 1);
        }
        
        private static char RandomLetter()
        {
            return (char)((int)'A' + Random0toN(25));
        }
        
        private static Random rand = new Random();
        
        private static readonly string[] lastNames      = { "Allen", "Aston", "Bartlett", "Brown",
                                                            "Cole", "Cutler", "Davidson", "Dunn",
                                                            "Ellis", "East", "Fenchurch", "Foster",
                                                            "Gardner", "Goldman", "Hall", "Horne",
                                                            "Innis", "Irwin", "Jackson", "Jones",
                                                            "Keller", "Keighley", "Lawson", "Lester",
                                                            "Maddison", "Morton", "Newton", "North",
                                                            "Old", "Oakley", "Peterson", "Pullman",
                                                            "Quinlan", "Quincy", "Rawlins", "Rowley",
                                                            "Smith", "Sells", "Tomkins", "Taylor",
                                                            "Unwin", "Upson", "Vardey", "Vickers",
                                                            "Watson", "Weston", "Yardley", "Zimmerman"
                                                        };
        private static readonly string[] firstNames = { "Anne", "Andrew", "Brenda", "Barney",
                                                            "Cathy", "Charles", "Dawn", "David",
                                                            "Ellie", "Edward", "Fern", "Frank",
                                                            "Geraldine", "George", "Helen", "Hector",
                                                            "Isobel", "Ian", "Jill", "Jack",
                                                            "Kelly", "Keith", "Lucy", "Lee",
                                                            "Maggie", "Michael", "Nancy", "Nigel",
                                                            "Orla", "Oswald", "Penny", "Peter",
                                                            "Rosie", "Rachel", "Robert", "Richard",
                                                            "Sophie", "Sally", "Simon", "Steven",
                                                            "Tara", "Tina", "Thomas", "Timothy",
                                                            "Ulrika", "Vincent", "Wesley", "Zoe"
                                                        };
        private static readonly string[] roadNames      = { "Alder", "Bank", "Castle", "Down", "East",
                                                            "Forest", "Grange", "Hills", "Inver", "Juniper",
                                                            "Kings", "Laurel", "Mansion", "North", "Oak",
                                                            "Parsonage", "Queens", "Railway", "South", "Thames",
                                                            "Underwood", "Vine", "West", "Yew Tree", "Zoo"
                                                        };
        private static readonly string[] roadTypes      = { "Avenue", "Road", "Street", "Cottages", "Lane",
                                                            "Close", "Way"
                                                        };
        private static readonly string[] bristolAreas   = { "Clifton", "Fishponds", "Bitton", "St Pauls" };
        private static readonly string[] bathAreas      = { "Weston", "Bear Flat", "Oldfield Park", "Combe Down",
                                                            "Landsdown", "Odd Down"
                                                        };
        private static readonly string[] deptNames      = { "General Surgery", "Radiology", "PAW", "ENT", "Gynae/Obs",
                                                            "Clerical", "Porterage", "Ancillary", "Paediatrics",
                                                            "Maxillo-Facial", "Dermatology", "Medical Imaging",
                                                            "Cardiology", "Physiotherapy", "A&E", "Other", "N/A"
                                                        };
    }
}