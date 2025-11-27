// islands/Dashboard.tsx
import { useEffect, useState } from "preact/hooks";

interface DataItem {
  id: number;
  title?: string;
  name?: string;
  email?: string;
}

export default function Dashboard() {
  const [activeTab, setActiveTab] = useState("users");
  const [data, setData] = useState<DataItem[]>([]);
  const [loading, setLoading] = useState(false);

  // When the tab changes, fetch new data
  useEffect(() => {
    const fetchData = async () => {
      setLoading(true);
      const res = await fetch(
        `https://jsonplaceholder.typicode.com/${activeTab}`,
      );
      const json = await res.json();
      setData(json.slice(0, 5)); // Limit to 5 for demo
      setLoading(false);
    };

    fetchData();
  }, [activeTab]);

  return (
    <div class="p-4 bg-white rounded shadow-md">
      {/* TAILWIND TABS */}
      <div class="flex border-b mb-4">
        {["users", "posts", "todos"].map((tab) => (
          <button
            type="button"
            onClick={() => setActiveTab(tab)}
            class={`px-4 py-2 capitalize font-medium ${
              activeTab === tab
                ? "border-b-2 border-blue-500 text-blue-600"
                : "text-gray-500 hover:text-blue-500"
            }`}
          >
            {tab}
          </button>
        ))}
      </div>

      {/* CONTENT AREA */}
      {loading
        ? <div class="p-4 text-blue-500 animate-pulse">Loading data...</div>
        : (
          <table class="min-w-full divide-y divide-gray-200">
            <thead class="bg-gray-50">
              <tr>
                <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                  ID
                </th>
                <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                  Title / Name
                </th>
              </tr>
            </thead>
            <tbody class="bg-white divide-y divide-gray-200">
              {data.map((item) => (
                <tr key={item.id}>
                  <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                    {item.id}
                  </td>
                  <td class="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                    {item.title || item.name}
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        )}
    </div>
  );
}
