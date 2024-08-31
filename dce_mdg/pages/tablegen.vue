<template>
  <div class="container">
    <div class="center mt-20">
      <UTable :columns="columns" :rows="rows">
        
      </UTable>
    </div>
    <button @click="generateAndSaveTable">Generate and Save Table</button>
    <footer class="footer">
      Michael Bressler • michael.bressler@tum.de
    </footer>
  </div>
</template>

<script setup lang="ts">
import { ref, onMounted } from 'vue';
import html2canvas from 'html2canvas';

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

const columns = ref([]);
const rows = ref([]);
const permutations = ref([]);
const currentPermutationIndex = ref(0);
const currentScenarioIndex = ref(0);

function generatePermutations(arr) {
  if (arr.length === 0) return [[]];
  const firstElem = arr[0];
  const rest = arr.slice(1);
  const permsWithoutFirst = generatePermutations(rest);
  const allPermutations = [];
  permsWithoutFirst.forEach(perm => {
    for (let i = 0; i <= perm.length; i++) {
      const permWithFirst = [...perm.slice(0, i), firstElem, ...perm.slice(i)];
      allPermutations.push(permWithFirst);
    }
  });
  return allPermutations;
}

function applyConstraints(permutations) {
  return permutations.filter(perm => {
    return true;
  });
}

function setTableData() {
  columns.value = [
    { key: 'attribute', label: scenarios[currentScenarioIndex.value] },
    ...alternatives.map((alt, index) => ({ key: `alt${index}`, label: alt }))
  ];
  const validPermutations = applyConstraints(generatePermutations(attributes));
  permutations.value = validPermutations;
  currentPermutationIndex.value = 0;
  updateTableData();
}

function updateTableData() {
  if (currentPermutationIndex.value < permutations.value.length) {
    const perm = permutations.value[currentPermutationIndex.value];
    rows.value = perm.map(attr => {
      const row = { attribute: attr.label };
      attr.levels.forEach((level, index) => {
        row[`alt${index}`] = level;
      });
      return row;
    });
  }
}

function saveTableAsPNG() {
  const tableElement = document.querySelector('.center');
  if (tableElement) {
    html2canvas(tableElement).then(canvas => {
      const link = document.createElement('a');
      link.download = `table_${currentScenarioIndex.value}_${currentPermutationIndex.value}.png`;
      link.href = canvas.toDataURL();
      link.click();
    });
  }
}

function generateAndSaveTable() {
  if (currentPermutationIndex.value < permutations.value.length) {
    updateTableData();
    saveTableAsPNG();
    currentPermutationIndex.value++;
  } else {
    currentPermutationIndex.value = 0;
    currentScenarioIndex.value = (currentScenarioIndex.value + 1) % scenarios.length;
    setTableData();
  }
}

onMounted(() => {
  setTableData();
});
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
