// routes/index.tsx
import { Head } from "$fresh/runtime.ts";
import Dashboard from "../islands/Dashboard.tsx";

export default function Home() {
  return (
    <>
      <Head>
        <title>PITA: Home</title>
      </Head>
      <div class="p-4 mx-auto max-w-screen-md bg-gray-100 min-h-screen">
        <h1 class="text-4xl font-bold mb-8 text-center text-gray-800">
          My Fresh App 🍋
        </h1>
        <Dashboard />
      </div>
    </>
  );
}
