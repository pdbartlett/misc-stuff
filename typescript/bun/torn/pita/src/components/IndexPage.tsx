import { QueryProvider } from './QueryProvider';
import StatusBar from './StatusBar';
import FactionTable from './FactionTable';

export function IndexPage() {
  return (
    <QueryProvider>
      <StatusBar />
      <div className="mt-6 card bg-base-100 shadow-xl">
        <div className="card-body">
          <h2 className="card-title">Faction Members</h2>
          <FactionTable />
        </div>
      </div>
    </QueryProvider>
  );
}
