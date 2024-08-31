<template>
  <div class="container">
    <div class="center mt-20">
      <UTable :rows="items" :columns="columns">
        <template #quantity-data="{ row }">
          {{ row.quantity.value }}
        </template>
      </UTable>
    </div>
    <button >Generate and Save Table</button>
    <footer class="footer">
      Michael Bressler • michael.bressler@tum.de
    </footer>
  </div>
</template>

<script setup lang="ts">

const scenarios = [
  "Commute/Leisure Trip",
  "Business Trip"
];

const alternatives = [
  "Integrated Route Planning",
  "Multiple Route Planners"
];

const attributes = [
  {
    label: "Total Travel Time",
    levels: [
      "20 Minutes",
      "35 Minutes",
      "50 Minutes"
    ]
  },
  {
    label: "Route Planning Experience",
    levels: [
      "Very Easy",
      "Manageable",
      "Cumbersome"
    ]
  },
  {
    label: "Personal data shared between providers",
    levels: [
      "Realtime Location Data",
      "Anonymized Data",
      "None"
    ]
  }
];

const columns = [{
  label: scenarios[0],
  class: 'italic'
}, {
  label: alternatives[0],
  class: 'italic'
}, {
  label: alternatives[1],
  class: 'italic'
}];

const rows = [{
  
}]

</script>

<style>
.container {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
}

.center {
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
  width: 100%;
}

.footer {
  padding: 20px;
  text-align: center;
  color: #8b8b8b;
}

iframe {
  border: none;
}
</style>
