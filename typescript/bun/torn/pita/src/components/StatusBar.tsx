import React from 'react';
import { useQueryClient } from '@tanstack/react-query';

const StatusBar = () => {
  const queryClient = useQueryClient();
  const [countdown, setCountdown] = React.useState(60);

  React.useEffect(() => {
    const timer = setInterval(() => {
      setCountdown(prev => (prev > 0 ? prev - 1 : 60));
    }, 1000);
    return () => clearInterval(timer);
  }, []);

  const handleRefresh = () => {
    queryClient.invalidateQueries();
    setCountdown(60);
  };

  return (
    <div className="flex items-center justify-end gap-4 p-2 bg-base-200 rounded-lg">
      <span className="text-sm">Next refresh in: {countdown}s</span>
      <button className="btn btn-xs btn-primary" onClick={handleRefresh}>
        Refresh Now
      </button>
    </div>
  );
};

export default StatusBar;
