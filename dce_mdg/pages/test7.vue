<script setup lang="ts">
const scenarios = {
  commuteLeisure: { label: "Commute/Leisure Trip", class: ''},
  businessTrip: { label: "Business Trip", class: '' }
};

const alternatives = {
  integratedRoutePlanning: { label: "Integrated Route Planning", class: 'bold bg-gray-100' },
  multipleRoutePlanners: { label: "Multiple Route Planners", class: 'bold bg-gray-100' }
};

const attributes = {
  time: {
    label: { 
      value: "Total Travel Time", 
      class: 'bg-gray-100', 
      icon: 'i-heroicons-clock'
    },
    levels: {
      min20: { value: "20 Minutes", class: 'text-green-600' },
      min35: { value: "35 Minutes", class: 'text-orange-500' },
      min50: { value: "50 Minutes", class: 'text-red-600' }
    }
  },
  experience: {
    label: { 
      value: "Route Planning Experience", 
      class: 'bg-gray-100',
      icon: 'i-heroicons-map'
    },
    levels: {
      veryEasy: { value: "🥰 Very Easy", class: '' },
      manageable: { value: "🙂 Manageable", class: '' },
      cumbersome: { value: "😒 Cumbersome", class: '' }
    }
  },
  datashared: {
    label: { 
      value: "Personal data shared", 
      class: 'bg-gray-100',
      icon: 'i-heroicons-lock-closed'
    },
    levels: {
      realtime: { value: "Realtime Location Data", class: 'font-bold' },
      anonymized: { value: "Anonymized Occupancy Data", class: '' },
      none: { value: "Nothing shared", class: 'font-bold' }
    }
  }
};

const columns = [{
  key: 'attribute',
  label: scenarios.commuteLeisure.label, 
  class: scenarios.commuteLeisure.class // Styling from scenarios
}, 
{
  key: 'alternative0',
  label: alternatives.integratedRoutePlanning.label,
  class: alternatives.integratedRoutePlanning.class // Styling from alternatives
},
 {
  key: 'alternative1',
  label: alternatives.multipleRoutePlanners.label,
  class: alternatives.multipleRoutePlanners.class // Styling from alternatives
}]

const items = [{
  attribute: attributes.time.label,
  alternative0: attributes.time.levels.min20,
  alternative1: attributes.time.levels.min20,
}, {
  attribute: attributes.experience.label,
  alternative0: attributes.experience.levels.veryEasy,
  alternative1: attributes.experience.levels.veryEasy,
}, {
  attribute: attributes.datashared.label,
  alternative0: attributes.datashared.levels.none,
  alternative1: attributes.datashared.levels.none,
}];

function updateAlternative(attributeName, alternativeIndex, newValue) {
  items.forEach(item => {
    if (item.attribute === attributeName) {
      if (alternativeIndex === 0) {
        item.alternative0 = newValue;
      } else if (alternativeIndex === 1) {
        item.alternative1 = newValue;
      }
    }
  });
}

function listAllPossibleInputs() {
  const timeLevels = [attributes.time.levels.min20, attributes.time.levels.min35, attributes.time.levels.min50];
  const experienceLevels = [attributes.experience.levels.veryEasy, attributes.experience.levels.managable, attributes.experience.levels.cumbersome];
  const datasharedLevels = [attributes.datashared.levels.none, attributes.datashared.levels.anonymized, attributes.datashared.levels.realtime];

  const possibleInputs = [];

  items.forEach(item => {
    const attributeName = item.attribute;
    let newValues = [];

    if (attributeName === attributes.time.label) {
      newValues = timeLevels;
    } else if (attributeName === attributes.experience.label) {
      newValues = experienceLevels;
    } else if (attributeName === attributes.datashared.label) {
      newValues = datasharedLevels;
    }

    newValues.forEach(newValue => {
      possibleInputs.push({ attributeName, alternativeIndex: 0, newValue });
      possibleInputs.push({ attributeName, alternativeIndex: 1, newValue });
    });
  });

  console.log("test")
  console.log(possibleInputs);
  
}

// Example usage:
listAllPossibleInputs();
</script>

<template>
  <UTable :rows="items" :columns="columns">
    <template #attribute-data="{ row }">
      <span :class="row.attribute.class" class="flex items-center">
        <UIcon :name="row.attribute.icon" class="w-5 h-5 mr-2" /> <!-- Displaying the Nuxt UI icon -->
        <span>{{ row.attribute.value }}</span>
      </span>
    </template>
    <template #alternative0-data="{ row }">
      <span :class="row.alternative0.class">{{ row.alternative0.value }}</span>
    </template>
    <template #alternative1-data="{ row }">
      <span :class="row.alternative1.class">{{ row.alternative1.value }}</span>
    </template>
  </UTable>
</template>

<style scoped>

</style>
