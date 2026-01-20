import React, { useMemo } from 'react';
import { useQuery } from '@tanstack/react-query';
import { useStore } from '@nanostores/react';
import {
  createColumnHelper,
  flexRender,
  getCoreRowModel,
  getSortedRowModel,
  useReactTable,
  type SortingState,
} from '@tanstack/react-table';
import { TornClient } from '../lib/api/tornClient';
import { apiKeys } from '../lib/store';

type FactionMember = {
  id: string;
  name: string;
  level: number;
  status: {
    description: string;
    details: string;
    state: string;
    color: string;
    until: number;
  };
};

const columnHelper = createColumnHelper<FactionMember>();

const columns = [
  columnHelper.accessor('name', {
    header: 'Name',
    cell: info => info.getValue(),
  }),
  columnHelper.accessor('level', {
    header: 'Level',
    cell: info => info.getValue(),
  }),
  columnHelper.accessor(row => row.status.description, {
    id: 'status',
    header: 'Status',
    cell: info => info.getValue(),
  }),
];

const FactionTable = () => {
  const $apiKeys = useStore(apiKeys);
  const [sorting, setSorting] = React.useState<SortingState>([]);

  const tornClient = useMemo(() => {
    if (!$apiKeys.torn) return null;
    return new TornClient($apiKeys.torn);
  }, [$apiKeys.torn]);

  const { data, isLoading, error } = useQuery({
    queryKey: ['factionMembers', $apiKeys.torn],
    queryFn: async () => {
        if (!tornClient) throw new Error("Torn client not available.");
        return tornClient.getFactionMembers();
    },
    enabled: !!$apiKeys.torn && !!tornClient,
  });

  const table = useReactTable({
    data: data ?? [],
    columns,
    state: { sorting },
    onSortingChange: setSorting,
    getCoreRowModel: getCoreRowModel(),
    getSortedRowModel: getSortedRowModel(),
  });

  if (isLoading) return <div className="text-center p-4">Loading faction members... <span className="loading loading-spinner"></span></div>;
  if (error) return <div className="text-center p-4 text-error">Error: {error instanceof Error ? error.message : 'An unknown error occurred'}</div>;

  return (
    <div className="overflow-x-auto">
      <table className="table table-zebra w-full">
        <thead>
          {table.getHeaderGroups().map(headerGroup => (
            <tr key={headerGroup.id}>
              {headerGroup.headers.map(header => (
                <th key={header.id} onClick={header.column.getToggleSortingHandler()} className="cursor-pointer">
                  {flexRender(header.column.columnDef.header, header.getContext())}
                  {{ asc: ' 🔼', desc: ' 🔽' }[header.column.getIsSorted() as string] ?? ''}
                </th>
              ))}
            </tr>
          ))}
        </thead>
        <tbody>
          {table.getRowModel().rows.map(row => (
            <tr key={row.id}>
              {row.getVisibleCells().map(cell => (
                <td key={cell.id}>
                  {flexRender(cell.column.columnDef.cell, cell.getContext())}
                </td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
};

export default FactionTable;
